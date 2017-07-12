#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifndef _M_PI
# define _M_PI 3.1415926535897932384626433832795028841971693993751058209
#endif

#define countof(Arr_) (sizeof(Arr_)/sizeof((Arr_)[0]))

const char *va( const char *format, ... )
{
	static char buf[ 65536 ];
	static size_t i = 0;
	va_list args;
	size_t n;

	char *p;

	p = &buf[ i ];

	va_start( args, format );
	n = vsnprintf( p, sizeof( buf ) - i, format, args );
	va_end( args );
	buf[ sizeof( buf ) - 1 ] = '\0';

	if( i + n > sizeof( buf ) - sizeof( buf )/128 ) {
		i = 0;
	} else {
		i += n;
	}

	return p;
}


/*
===============================================================================

	MEMORY SYSTEM

	- Allows attaching of memory objects to other memory objects, which will
	  free all sub-allocations when the super-allocation is freed.
	- Memory objects can have an "at-free" function pointer, which will be
	  invoked upon freeing the memory.
	- Reference counting is enabled on memory objects. Memory will only be
	  freed once all reference counts reach zero.

===============================================================================
*/

typedef void( *pfn_fini_t )( void * );

typedef struct alloc_s {
	size_t n;
	size_t refcnt;
	struct alloc_s *a_prnt;
	struct alloc_s *a_head;
	struct alloc_s *a_tail;
	struct alloc_s *a_prev;
	struct alloc_s *a_next;
	pfn_fini_t pfn_fini;
} alloc_t;

static void mem__unlink_( alloc_t *a )
{
	if( a->a_prev != NULL ) {
		a->a_prev->a_next = a->a_next;
	} else if( a->a_prnt != NULL ) {
		a->a_prnt->a_head = a->a_next;
	}
	if( a->a_next != NULL ) {
		a->a_next->a_prev = a->a_prev;
	} else if( a->a_prnt != NULL ) {
		a->a_prnt->a_tail = a->a_prev;
	}
}

void *mem_alloc( size_t n )
{
	void *p;

	p = malloc( n + sizeof( alloc_t ) );

	( ( alloc_t * )p )->n = n;
	( ( alloc_t * )p )->refcnt = 1;
	( ( alloc_t * )p )->a_prnt = NULL;
	( ( alloc_t * )p )->a_head = NULL;
	( ( alloc_t * )p )->a_tail = NULL;
	( ( alloc_t * )p )->a_prev = NULL;
	( ( alloc_t * )p )->a_next = NULL;
	( ( alloc_t * )p )->pfn_fini = NULL;

	memset( ( void * )( ( alloc_t * )p + 1 ), 0, n );

	return ( void * )( ( alloc_t * )p + 1 );
}
void *mem_free( void *p )
{
	alloc_t *a;

	if( !p ) {
		return NULL;
	}

	a = ( alloc_t * )p - 1;
	if( --a->refcnt != 0 ) {
		return NULL;
	}

	mem__unlink_( a );

	if( a->pfn_fini != NULL ) {
		a->pfn_fini( p );
	}

	while( a->a_head != NULL ) {
		mem_free( ( void * )( a->a_head + 1 ) );
	}

	free( ( void * )a );
	return NULL;
}
void *mem_addref( void *p )
{
	assert( p != NULL );

	++( ( alloc_t * )p - 1 )->refcnt;
	return p;
}
void *mem_atfree( void *p, pfn_fini_t pfn_fini )
{
	assert( p != NULL );

	( ( alloc_t * )p - 1 )->pfn_fini = pfn_fini;
	return p;
}
size_t mem_size( const void *p )
{
	if( !p ) {
		return 0;
	}

	return ( ( alloc_t * )p - 1 )->n;
}
void *mem_detach( void *subobject )
{
	alloc_t *chld;

	assert( subobject != NULL );

	chld = ( alloc_t * )subobject - 1;

	mem__unlink_( chld );
	return subobject;
}
void *mem_attach( void *subobject, void *superobject )
{
	alloc_t *chld, *prnt;

	assert( subobject != NULL );
	assert( superobject != NULL );

	chld = ( alloc_t * )subobject - 1;
	prnt = ( alloc_t * )superobject - 1;

	mem__unlink_( chld );

	chld->a_prnt = prnt;
	chld->a_prev = prnt->a_tail;
	chld->a_next = NULL;
	if( chld->a_prev != NULL ) {
		chld->a_prev->a_next = chld;
	} else {
		prnt->a_head = chld;
	}
	prnt->a_tail = chld;

	return subobject;
}


char *mem_strdup( const char *s )
{
	size_t n;
	void *p;

	n = 1 + ( s != NULL ? strlen( s ) : 0 );

	p = mem_alloc( n );
	if( !p ) {
		return NULL;
	}

	if( s != NULL ) {
		memcpy( p, ( const void * )s, n );
	} else {
		*( char * )p = '\0';
	}
	return ( char * )p;
}
char *mem_strdupn( const char *s, size_t max )
{
	size_t n;
	void *p;

	n = 0;
	if( s != NULL ) {
		while( n < max ) {
			if( s[n] == '\0' ) {
				break;
			}

			++n;
		}
	}

	p = mem_alloc( n + 1 );
	if( !p ) {
		return NULL;
	}

	if( s != NULL ) {
		memcpy( p, ( const void * )s, n );
	}
	( ( char * )p )[ n ] = '\0';

	return ( char * )p;
}


/*
===============================================================================

	VIRTUAL MACHINE

===============================================================================
*/

typedef enum vmopcode_e {
	vmop_return,
	vmop_var_const,
	vmop_var_decl,
	vmop_store,
	vmop_push_var,
	vmop_push_lit,
	vmop_neg,
	vmop_add,
	vmop_sub,
	vmop_mul,
	vmop_div,
	vmop_pow,
	vmop_mod,
	vmop_call,

	vmops
} vmopcode_t;

typedef enum vmstatus_e {
	vmstat_ok,
	vmstat_end,

	vmstat_err_stack_underflow = -1,
	vmstat_err_stack_overflow = -2,
	vmstat_err_invalid_reg = -3,
	vmstat_err_read_only = -4,
	vmstat_err_invalid_call = -5,
	vmstat_err_invalid_instruction = -6
} vmstatus_t;

typedef struct vminstruction_s {
	vmopcode_t op;
	union {
		double value;
		unsigned int slot;
	} operand;
} vminstruction_t;

typedef double( *pfn_vmfunc0_t )( void );
typedef double( *pfn_vmfunc1_t )( double );
typedef double( *pfn_vmfunc2_t )( double, double );
typedef double( *pfn_vmfunc3_t )( double, double, double );
typedef double( *pfn_vmfunc4_t )( double, double, double, double );
typedef double( *pfn_vmfunc5_t )( double, double, double, double, double );
typedef double( *pfn_vmfunc6_t )( double, double, double, double, double, double );
typedef double( *pfn_vmfunc7_t )( double, double, double, double, double, double, double );
typedef double( *pfn_vmfunc8_t )( double, double, double, double, double, double, double, double );
typedef double( *pfn_vmfunc9_t )( double, double, double, double, double, double, double, double, double );

typedef struct vmbuiltinfunc_s {
	unsigned int numparms;
	union {
		void *p;
		pfn_vmfunc0_t pfn0;
		pfn_vmfunc1_t pfn1;
		pfn_vmfunc2_t pfn2;
		pfn_vmfunc3_t pfn3;
		pfn_vmfunc4_t pfn4;
		pfn_vmfunc5_t pfn5;
		pfn_vmfunc6_t pfn6;
		pfn_vmfunc7_t pfn7;
		pfn_vmfunc8_t pfn8;
		pfn_vmfunc9_t pfn9;
	} func;
} vmbuiltinfunc_t;

typedef struct vmcontext_s {
	unsigned int numregs;
	double *regs;
	int *regflags;

	vmbuiltinfunc_t funcs[ 128 ];

	double stack[ 128 ];
	size_t stackp;

	double value;

	const vminstruction_t *instr_s;
	const vminstruction_t *instr_e;

	const vminstruction_t *p;
} vmcontext_t;

int vminit( vmcontext_t *vm, unsigned int numregs )
{
	assert( vm != NULL );

	vm->numregs = numregs;

	vm->regs = ( double * )mem_alloc( numregs*sizeof( double ) );
	if( !vm->regs ) {
		return 0;
	}

	vm->regflags = ( int * )mem_alloc( numregs*sizeof( int ) );
	if( !vm->regflags ) {
		vm->regs = ( double * )mem_free( ( void * )vm->regs );
		return 0;
	}

	mem_attach( ( void * )vm->regflags, ( void * )vm->regs );

	memset( &vm->funcs[ 0 ], 0, sizeof( vm->funcs ) );

	vm->stackp = 0;
	vm->value = 0;

	vm->instr_s = NULL;
	vm->instr_e = NULL;

	vm->p = NULL;
	return 1;
}
void vmfini( vmcontext_t *vm )
{
	assert( vm != NULL );

	mem_free( ( void * )vm->regs );
	vm->regs = NULL;
	vm->regflags = NULL;
}
vmstatus_t vmstep( vmcontext_t *vm )
{
	const vminstruction_t *p;

	assert( vm != NULL );
	assert( !vm->numregs || vm->regs != NULL );
	assert( !vm->numregs || vm->regflags != NULL );
	assert( vm->instr_s != NULL );
	assert( vm->instr_e != NULL );

	if( !vm->p ) {
		vm->p = vm->instr_s;
		vm->stackp = 0;
		vm->value = 0;
	}

	if( vm->p == vm->instr_e ) {
		return vmstat_end;
	}

	p = vm->p++;

	switch( p->op ) {
	case vmop_return:
		if( vm->stackp > 0 ) {
			vm->value = vm->stack[ --vm->stackp ];
		} else {
			return vmstat_err_stack_underflow;
		}
		vm->p = NULL;
		return vmstat_end;

	case vmop_var_const:
		if( p->operand.slot >= vm->numregs ) {
			return vmstat_err_invalid_reg;
		}
		vm->regs[ p->operand.slot ] = 0;
		vm->regflags[ p->operand.slot ] = 1;
		return vmstat_ok;

	case vmop_var_decl:
		if( p->operand.slot >= vm->numregs ) {
			return vmstat_err_invalid_reg;
		}
		vm->regs[ p->operand.slot ] = 0;
		vm->regflags[ p->operand.slot ] = 0;
		return vmstat_ok;

	case vmop_store:
		if( p->operand.slot >= vm->numregs ) {
			return vmstat_err_invalid_reg;
		}
		if( vm->stackp == 0 ) {
			return vmstat_err_stack_underflow;
		}
		if( vm->regflags[ p->operand.slot ] & 2 ) {
			return vmstat_err_read_only;
		} else if( vm->regflags[ p->operand.slot ] & 1 ) {
			vm->regflags[ p->operand.slot ] |= 2;
		}

		vm->regs[ p->operand.slot ] = vm->stack[ --vm->stackp ];
		return vmstat_ok;

	case vmop_push_var:
		if( p->operand.slot >= vm->numregs ) {
			return vmstat_err_invalid_reg;
		}
		if( vm->stackp == countof( vm->stack ) ) {
			return vmstat_err_stack_overflow;
		}

		vm->stack[ vm->stackp++ ] = vm->regs[ p->operand.slot ];
		return vmstat_ok;
	case vmop_push_lit:
		if( vm->stackp == countof( vm->stack ) ) {
			return vmstat_err_stack_overflow;
		}

		vm->stack[ vm->stackp++ ] = p->operand.value;
		return vmstat_ok;

	case vmop_neg:
		if( vm->stackp < 1 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 1 ] = -vm->stack[ vm->stackp - 1 ];
		return vmstat_ok;

	case vmop_add:
		if( vm->stackp < 2 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 2 ] = vm->stack[ vm->stackp - 2 ] + vm->stack[ vm->stackp - 1 ];
		--vm->stackp;
		return vmstat_ok;
	case vmop_sub:
		if( vm->stackp < 2 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 2 ] = vm->stack[ vm->stackp - 2 ] - vm->stack[ vm->stackp - 1 ];
		--vm->stackp;
		return vmstat_ok;
	case vmop_mul:
		if( vm->stackp < 2 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 2 ] = vm->stack[ vm->stackp - 2 ]*vm->stack[ vm->stackp - 1 ];
		--vm->stackp;
		return vmstat_ok;
	case vmop_div:
		if( vm->stackp < 2 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 2 ] = vm->stack[ vm->stackp - 2 ]/vm->stack[ vm->stackp - 1 ];
		--vm->stackp;
		return vmstat_ok;

	case vmop_pow:
		if( vm->stackp < 2 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 2 ] = pow( vm->stack[ vm->stackp - 2 ], vm->stack[ vm->stackp - 1 ] );
		--vm->stackp;
		return vmstat_ok;
	case vmop_mod:
		if( vm->stackp < 2 ) {
			return vmstat_err_stack_underflow;
		}

		vm->stack[ vm->stackp - 2 ] = fmod( vm->stack[ vm->stackp - 2 ], vm->stack[ vm->stackp - 1 ] );
		--vm->stackp;
		return vmstat_ok;

	case vmop_call:
		if( p->operand.slot >= ( unsigned int )countof( vm->funcs ) ) {
			return vmstat_err_invalid_call;
		}
		if( vm->stackp < vm->funcs[ p->operand.slot ].numparms ) {
			return vmstat_err_stack_underflow;
		}
		if( !vm->funcs[ p->operand.slot ].func.p ) {
			return vmstat_err_invalid_call;
		}
		if( vm->stackp >= countof( vm->stack ) - vm->funcs[ p->operand.slot ].numparms ) {
			return vmstat_err_stack_overflow;
		}

		switch( vm->funcs[ p->operand.slot ].numparms ) {
		case 0:
			vm->stack[ vm->stackp++ ] =
				vm->funcs[ p->operand.slot ].func.pfn0();
			break;

		case 1:
			vm->stack[ vm->stackp - 1 ] =
				vm->funcs[ p->operand.slot ].func.pfn1(
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 0;
			break;
		case 2:
			vm->stack[ vm->stackp - 2 ] =
				vm->funcs[ p->operand.slot ].func.pfn2(
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 1;
			break;
		case 3:
			vm->stack[ vm->stackp - 3 ] =
				vm->funcs[ p->operand.slot ].func.pfn3(
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 2;
			break;
		case 4:
			vm->stack[ vm->stackp - 4 ] =
				vm->funcs[ p->operand.slot ].func.pfn4(
					vm->stack[ vm->stackp - 4 ],
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 3;
			break;
		case 5:
			vm->stack[ vm->stackp - 5 ] =
				vm->funcs[ p->operand.slot ].func.pfn5(
					vm->stack[ vm->stackp - 5 ],
					vm->stack[ vm->stackp - 4 ],
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 4;
			break;
		case 6:
			vm->stack[ vm->stackp - 6 ] =
				vm->funcs[ p->operand.slot ].func.pfn6(
					vm->stack[ vm->stackp - 6 ],
					vm->stack[ vm->stackp - 5 ],
					vm->stack[ vm->stackp - 4 ],
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 5;
			break;
		case 7:
			vm->stack[ vm->stackp - 7 ] =
				vm->funcs[ p->operand.slot ].func.pfn7(
					vm->stack[ vm->stackp - 7 ],
					vm->stack[ vm->stackp - 6 ],
					vm->stack[ vm->stackp - 5 ],
					vm->stack[ vm->stackp - 4 ],
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 6;
			break;
		case 8:
			vm->stack[ vm->stackp - 8 ] =
				vm->funcs[ p->operand.slot ].func.pfn8(
					vm->stack[ vm->stackp - 8 ],
					vm->stack[ vm->stackp - 7 ],
					vm->stack[ vm->stackp - 6 ],
					vm->stack[ vm->stackp - 5 ],
					vm->stack[ vm->stackp - 4 ],
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 7;
			break;
		case 9:
			vm->stack[ vm->stackp - 9 ] =
				vm->funcs[ p->operand.slot ].func.pfn9(
					vm->stack[ vm->stackp - 9 ],
					vm->stack[ vm->stackp - 8 ],
					vm->stack[ vm->stackp - 7 ],
					vm->stack[ vm->stackp - 6 ],
					vm->stack[ vm->stackp - 5 ],
					vm->stack[ vm->stackp - 4 ],
					vm->stack[ vm->stackp - 3 ],
					vm->stack[ vm->stackp - 2 ],
					vm->stack[ vm->stackp - 1 ]
				);
			vm->stackp -= 8;
			break;
		default:
			assert( 0 && "Too many parameters" );
			return vmstat_err_invalid_call;
		}
		
		return vmstat_ok;

	default:
		assert( 0 && "Invalid instruction" );
		return vmstat_err_invalid_instruction;
	}
}

double vmeval( vmcontext_t *vm )
{
	vmstatus_t r;

	assert( vm != NULL );

	for(;;) {
		r = vmstep( vm );
		if( r < vmstat_ok ) {
			fprintf( stderr, "ERROR: vmstep() failed with code %i\n", ( int )r );
			return -1.0;
		}

		if( r == vmstat_end ) {
			break;
		}
	}

	return vm->value;
}


/*
===============================================================================

	BUILT-IN FUNCTIONS

===============================================================================
*/

double builtin__sin_( double x )
{
	return sin( x );
}
double builtin__cos_( double x )
{
	return cos( x );
}

double builtin__exp_( double x )
{
	return exp( x );
}
double builtin__log_( double x )
{
	return log( x );
}
double builtin__atan_( double x )
{
	return atan( x );
}
double builtin__atan2_( double y, double x )
{
	return atan2( y, x );
}
double builtin__pow_( double x, double y )
{
	return pow( x, y );
}
double builtin__fmod_( double x, double y )
{
	return fmod( x, y );
}

double builtin__lerp_( double a, double b, double t )
{
	return a + ( b - a )*t;
}
double builtin__cerp_( double x, double y, double z, double w, double t )
{
	double a, b, c;

	a = ( w - z ) - ( x - y );
	b = ( x - y ) - a;
	c = ( z - x );

	return t*( t*( t*a + b ) + c ) + y;
}
double builtin__slerp_( double a, double b, double t )
{
	double factor[ 2 ];
	double cosangle;
	double angle;
	double sinangle;

	if( t <= 0.0 ) { return a; }
	if( t >= 1.0 ) { return b; }

	factor[0] = 1.0 - t;
	factor[1] = t;

	cosangle = a*b; /*dot*/
	if( 1.0 - cosangle > 1e-8f ) {
		angle = acos( cosangle );
		sinangle = sin( angle );

		factor[0] = sin( factor[0]*angle )/sinangle;
		factor[1] = sin( factor[1]*angle )/sinangle;
	}

	return a*factor[0] + b*factor[1];
}
double builtin__sign_( double x )
{
	return x < 0 ? -1 : x > 0 ? +1 : 0;
}
double builtin__frac_( double x )
{
	return x - floor( x );
}
double builtin__tween_( double a, double b, double t )
{
	return ( t - a )/( b - a );
}
double builtin__curve_( double a, double b, double t )
{
	return sin( _M_PI*builtin__tween_( a, b, t ) );
}
double builtin__smooth_step_( double a, double b, double x )
{
	double y;

	if( x < a ) { return 0; }
	if( x > b ) { return 1; }

	y = builtin__tween_( a, b, x );
	return y*y*( 3.0 - 2.0*y );
}
double builtin__step_( double a, double x )
{
	return x < a ? 0.0 : 1.0;
}
double builtin__over_( double x, double y )
{
	return 1.0 - ( 1.0 - x )*( 1.0 - y );
}

double builtin__tri_( double a, double x )
{
	double y2, y1, y;

	y2 = builtin__frac_( x/( 2.0*_M_PI ) );
	y1 = y2 < 0.0 ? 1.0 + y2 : y2;
	y = y1 < a ? y1/a : 1.0 - builtin__tween_( a, 1, y1 );
	return -1.0 + 2.0*y;
}
double builtin__saw_( double a, double x )
{
	double f;

	f = builtin__frac_( x );
	return f < a ? f/a : 1.0 - ( f - a )/( 1.0 - a );
}
double builtin__square_( double a, double x )
{
	return sin( x ) > a ? 1.0 : -1.0;
}

static double grad_( int n, double x )
{
	n = ( n << 13 ) ^ n;
	n = ( n*( n*n*15731 + 789221 ) + 1376312589 );

	return ( n & 0x20000000 ) ? -x : x;
}
double builtin__grad_( double n, double x )
{
	return grad_( ( int )n, x );
}
double builtin__noise_( double x )
{
	double f, w, a, b;
	int i;

	i = ( int )x;
	f = builtin__frac_( x );
	w = f*f*f*( f*( f*6.0 - 15.0 ) + 10.0 );
	a = grad_( i + 0, f + 0.0 );
	b = grad_( i + 1, f - 1.0 );

	return a + ( b - a )*w;
}

double builtin__clamp_( double x, double a, double b )
{
	return x < a ? a : ( x > b ? b : x );
}
double builtin__clamp_snorm_( double x )
{
	return builtin__clamp_( x, -1.0, 1.0 );
}
double builtin__clamp_unorm_( double x )
{
	return builtin__clamp_( x, 0.0, 1.0 );
}
double builtin__max_( double a, double b )
{
	return a > b ? a : b;
}
double builtin__min_( double a, double b )
{
	return a < b ? a : b;
}

double builtin__bark_( double y )
{
	return 13*atan( 0.00076*y ) + 3.5*atan( ( y/7500.0 )*( y/7500.0 ) )*70.0;
}


/*
===============================================================================

	LEXER AND PARSER

===============================================================================
*/

typedef enum toktype_e {
	tok_eof,

	tok_name,
	tok_number,

	tok_lparen = '(',
	tok_rparen = ')',
	tok_add = '+',
	tok_sub = '-',
	tok_mul = '*',
	tok_div = '/',
	tok_mod = '%',
	tok_pow = '^',
	tok_semicolon = ';',
	tok_comma = ',',
	tok_eq = '='
} toktype_t;

typedef enum unaryop_e {
	unary_none,

	unary_negate
} unaryop_t;
typedef enum binaryop_e {
	binop_none,

	binop_add,
	binop_sub,
	binop_mul,
	binop_div,
	binop_mod,
	binop_pow
} binaryop_t;

typedef enum builtinfunc_e {
	intfn_none,

	intfn_sin,
	intfn_cos,
	intfn_exp,
	intfn_log,
	intfn_atan,
	intfn_atan2,
	intfn_pow,
	intfn_fmod,
	intfn_lerp,
	intfn_cerp,
	intfn_slerp,
	intfn_sign,
	intfn_frac,
	intfn_tween,
	intfn_curve,
	intfn_smooth_step,
	intfn_step,
	intfn_over,
	intfn_tri,
	intfn_saw,
	intfn_square,
	intfn_grad,
	intfn_noise,
	intfn_clamp,
	intfn_clamp_snorm,
	intfn_clamp_unorm,
	intfn_max,
	intfn_min,
	intfn_bark
} builtinfunc_t;

struct token_s;
struct source_s;

typedef struct token_s {
	toktype_t type;
	double number;
	const char *name_s;
	const char *name_e;
	struct source_s *source;
	struct token_s *t_prev;
	struct token_s *t_next;
} token_t;

typedef struct source_s {
	char *filename;
	char *text;
	const char *p;
	struct token_s *t_head;
	struct token_s *t_tail;
	struct token_s *t_unread;
} source_t;

typedef struct vardecl_s {
	token_t *decltok;
	int is_const;
	unsigned int vm_index;
	struct vardecl_s *v_prev;
	struct vardecl_s *v_next;
} vardecl_t;
typedef struct vmfunc_s {
	char *name;
	unsigned int num_parms;
	builtinfunc_t intfunc;
} vmfunc_t;

static const vmfunc_t g_vmfuncs[] = {
	{ "sin", 1, intfn_sin },
	{ "cos", 1, intfn_cos },
	{ "exp", 1, intfn_exp },
	{ "log", 1, intfn_log },
	{ "atan", 1, intfn_atan },
	{ "atan2", 2, intfn_atan2 },
	{ "pow", 2, intfn_pow },
	{ "fmod", 2, intfn_fmod },
	{ "lerp", 3, intfn_lerp },
	{ "mix", 3, intfn_lerp },
	{ "cerp", 5, intfn_cerp },
	{ "slerp", 3, intfn_slerp },
	{ "sign", 1, intfn_sign },
	{ "frac", 1, intfn_frac },
	{ "fract", 1, intfn_frac },
	{ "tween", 3, intfn_tween },
	{ "curve", 3, intfn_curve },
	{ "smoothstep", 3, intfn_smooth_step },
	{ "step", 2, intfn_step },
	{ "over", 2, intfn_over },
	{ "tri", 2, intfn_tri },
	{ "saw", 2, intfn_saw },
	{ "square", 2, intfn_square },
	{ "grad", 2, intfn_grad },
	{ "noise", 1, intfn_noise },
	{ "clamp", 3, intfn_clamp },
	{ "clamp_snorm", 1, intfn_clamp_snorm },
	{ "clamp_unorm", 1, intfn_clamp_unorm },
	{ "saturate", 1, intfn_clamp_unorm },
	{ "max", 2, intfn_max },
	{ "min", 2, intfn_min },
	{ "bark", 1, intfn_bark }
};
void *getbuiltinproc( builtinfunc_t intfunc )
{
	switch( intfunc )
	{
	case intfn_none:		return NULL;
	case intfn_sin:			return ( void * )&builtin__sin_;
	case intfn_cos:			return ( void * )&builtin__cos_;
	case intfn_exp:			return ( void * )&builtin__exp_;
	case intfn_log:			return ( void * )&builtin__log_;
	case intfn_atan:		return ( void * )&builtin__atan_;
	case intfn_atan2:		return ( void * )&builtin__atan2_;
	case intfn_pow:			return ( void * )&builtin__pow_;
	case intfn_fmod:		return ( void * )&builtin__fmod_;
	case intfn_lerp:		return ( void * )&builtin__lerp_;
	case intfn_cerp:		return ( void * )&builtin__cerp_;
	case intfn_slerp:		return ( void * )&builtin__slerp_;
	case intfn_sign:		return ( void * )&builtin__sign_;
	case intfn_frac:		return ( void * )&builtin__frac_;
	case intfn_tween:		return ( void * )&builtin__tween_;
	case intfn_curve:		return ( void * )&builtin__curve_;
	case intfn_smooth_step:	return ( void * )&builtin__smooth_step_;
	case intfn_step:		return ( void * )&builtin__step_;
	case intfn_over:		return ( void * )&builtin__over_;
	case intfn_tri:			return ( void * )&builtin__tri_;
	case intfn_saw:			return ( void * )&builtin__saw_;
	case intfn_square:		return ( void * )&builtin__square_;
	case intfn_grad:		return ( void * )&builtin__grad_;
	case intfn_noise:		return ( void * )&builtin__noise_;
	case intfn_clamp:		return ( void * )&builtin__clamp_;
	case intfn_clamp_snorm:	return ( void * )&builtin__clamp_snorm_;
	case intfn_clamp_unorm:	return ( void * )&builtin__clamp_unorm_;
	case intfn_max:			return ( void * )&builtin__max_;
	case intfn_min:			return ( void * )&builtin__min_;
	case intfn_bark:		return ( void * )&builtin__bark_;
	}

	assert( 0 && "Unreachable (unhandled builtinfunc_t)" );
	return NULL;
}

void install_vm_funcs( vmcontext_t *vm )
{
	size_t i;
	size_t j;

	vm->funcs[ 0 ].numparms = 0;
	vm->funcs[ 0 ].func.p = NULL;

	for( i = 0; i < countof( g_vmfuncs ); ++i ) {
		j = ( size_t )g_vmfuncs[ i ].intfunc;
		vm->funcs[ j ].numparms = g_vmfuncs[ i ].num_parms;
		vm->funcs[ j ].func.p = getbuiltinproc( g_vmfuncs[ i ].intfunc );
	}
}

struct stmt_s;
struct expr_s;
struct program_s;

typedef int( *pfn_stmt_semant_t )( struct stmt_s *, struct program_s * );
typedef int( *pfn_stmt_codegen_t )( struct stmt_s *, struct program_s * );

typedef int( *pfn_expr_semant_t )( struct expr_s *, struct program_s * );
typedef int( *pfn_expr_codegen_t )( struct expr_s *, struct program_s * );

typedef struct stmt_s {
	pfn_stmt_semant_t pfn_semant;
	pfn_stmt_codegen_t pfn_codegen;
	struct stmt_s *s_prev;
	struct stmt_s *s_next;
} stmt_t;

typedef struct expr_s {
	pfn_expr_semant_t pfn_semant;
	pfn_expr_codegen_t pfn_codegen;
	struct expr_s *e_prev;
	struct expr_s *e_next;
} expr_t;

typedef struct expr_literal_s {
	token_t *tok;
} expr_literal_t;
typedef struct expr_varname_s {
	token_t *vartok;

	vardecl_t *semanted__decl;
} expr_varname_t;
typedef struct expr_callfunc_s {
	token_t *fntok;
	expr_t *parms[ 9 ];

	const vmfunc_t *semanted__decl;
} expr_callfunc_t;
typedef struct expr_unaryop_s {
	unaryop_t op;
	expr_t *subexpr;
} expr_unaryop_t;
typedef struct expr_binaryop_s {
	binaryop_t op;
	expr_t *left;
	expr_t *right;
} expr_binaryop_t;

typedef struct stmt_vardecl_s {
	token_t *vartok;
	expr_t *defexpr;
	int is_const;

	vardecl_t *semanted__decl;
} stmt_vardecl_t;
typedef struct stmt_varassign_s {
	token_t *vartok;
	expr_t *defexpr;

	binaryop_t compoundop;

	vardecl_t *semanted__decl;
} stmt_varassign_t;
typedef struct stmt_return_s {
	expr_t *retexpr;
} stmt_return_t;

typedef struct program_s {
	source_t *source;

	vardecl_t *v_head;
	vardecl_t *v_tail;

	stmt_t *s_head;
	stmt_t *s_tail;

	unsigned int vm_indexes;

	FILE *fp_asm;
	size_t num_instrs;
	vminstruction_t *instrs;
} program_t;

typedef struct operator_s {
	toktype_t tok;
	unsigned char precedence; 
	binaryop_t binop;
} operator_t;

static const unsigned char g_unaryop_precedence = 250;
static const operator_t g_operators[] = {
	{ tok_add, 140, binop_add },
	{ tok_sub, 140, binop_sub },

	{ tok_mul, 150, binop_mul },
	{ tok_div, 150, binop_div },
	{ tok_mod, 150, binop_mod },

	{ tok_pow, 160, binop_pow }
};

void tok_error( const token_t *tok, const char *errormsg );

int tok_cmp( const token_t *tok, const char *text )
{
	size_t n;

	assert( tok != NULL );
	assert( tok->name_s != NULL );
	assert( tok->name_e != NULL );
	assert( text != NULL );

	n = strlen( text );
	if( ( size_t )( tok->name_e - tok->name_s ) != n ) {
		return 0;
	}

	return strncmp( tok->name_s, text, n ) == 0;
}
int tok_cmpn( const token_t *tok, const char *text, size_t ntext )
{
	size_t n;

	assert( tok != NULL );
	assert( tok->name_s != NULL );
	assert( tok->name_e != NULL );
	assert( text != NULL );

	n = ( size_t )( tok->name_e - tok->name_s );
	if( ntext != n ) {
		return 0;
	}

	return strncmp( tok->name_s, text, n ) == 0;
}

vardecl_t *prog_find_var_n( const program_t *prog, const char *varname, size_t nvarname )
{
	vardecl_t *v;

	assert( prog != NULL );
	assert( varname != NULL );

	for( v = prog->v_head; v != NULL; v = v->v_next ) {
		assert( v->decltok != NULL );

		if( tok_cmpn( v->decltok, varname, nvarname ) ) {
			return v;
		}
	}

	return NULL;
}
vardecl_t *prog_find_var( const program_t *prog, const char *varname )
{
	return prog_find_var_n( prog, varname, varname != NULL ? strlen( varname ) : 0 );
}
vardecl_t *prog_find_var_tok( const program_t *prog, const token_t *vartok )
{
	assert( vartok != NULL );
	assert( vartok->name_s != NULL );
	assert( vartok->name_e != NULL );

	return prog_find_var_n( prog, vartok->name_s, ( size_t )( vartok->name_e - vartok->name_s ) );
}

const vmfunc_t *find_vmfunc_n( const char *funcname, size_t nfuncname )
{
	size_t i;

	assert( funcname != NULL );

	for( i = 0; i < countof( g_vmfuncs ); ++i ) {
		assert( g_vmfuncs[ i ].name != NULL );

		if( strncmp( g_vmfuncs[ i ].name, funcname, nfuncname ) != 0 ) {
			continue;
		}

		if( strlen( g_vmfuncs[ i ].name ) == nfuncname ) {
			return &g_vmfuncs[ i ];
		}
	}

	return NULL;
}
const vmfunc_t *find_vmfunc( const char *funcname )
{
	return find_vmfunc_n( funcname, funcname != NULL ? strlen( funcname ) : 0 );
}
const vmfunc_t *find_vmfunc_tok( const token_t *nametok )
{
	assert( nametok != NULL );
	assert( nametok->name_s != NULL );
	assert( nametok->name_e != NULL );

	return find_vmfunc_n( nametok->name_s, ( size_t )( nametok->name_e - nametok->name_s ) );
}

vminstruction_t *prog_new_instr( program_t *prog )
{
	static const size_t gran = 64;
	size_t max_instrs;

	max_instrs = prog->num_instrs + ( prog->num_instrs%gran != 0 ? gran - prog->num_instrs%gran : 0 );

	if( prog->num_instrs + 1 > max_instrs ) {
		void *p;

		p = mem_alloc( ( max_instrs + gran )*sizeof( vminstruction_t ) );
		if( !p ) {
			return NULL;
		}

		mem_attach( p, ( void * )prog );

		if( prog->instrs != NULL ) {
			memcpy( p, ( const void * )prog->instrs, prog->num_instrs*sizeof( vminstruction_t ) );
			mem_free( ( void * )prog->instrs );
		}

		prog->instrs = ( vminstruction_t * )p;
	}

	return &prog->instrs[ prog->num_instrs++ ];
}
vminstruction_t *prog_emit_instr( program_t *prog, vmopcode_t op )
{
	vminstruction_t *p;

	p = prog_new_instr( prog );
	if( !p ) {
		return NULL;
	}

	p->op = op;
	return p;
}
vminstruction_t *prog_emit_return( program_t *prog )
{
	return prog_emit_instr( prog, vmop_return );
}
vminstruction_t *prog_emit_var_const( program_t *prog, unsigned int index )
{
	vminstruction_t *p;

	p = prog_emit_instr( prog, vmop_var_const );
	if( !p ) {
		return NULL;
	}

	p->operand.slot = index;
	return p;
}
vminstruction_t *prog_emit_var_decl( program_t *prog, unsigned int index )
{
	vminstruction_t *p;

	p = prog_emit_instr( prog, vmop_var_decl );
	if( !p ) {
		return NULL;
	}

	p->operand.slot = index;
	return p;
}
vminstruction_t *prog_emit_store( program_t *prog, unsigned int index )
{
	vminstruction_t *p;

	p = prog_emit_instr( prog, vmop_store );
	if( !p ) {
		return NULL;
	}

	p->operand.slot = index;
	return p;
}
vminstruction_t *prog_emit_push_var( program_t *prog, unsigned int index )
{
	vminstruction_t *p;

	p = prog_emit_instr( prog, vmop_push_var );
	if( !p ) {
		return NULL;
	}

	p->operand.slot = index;
	return p;
}
vminstruction_t *prog_emit_push_lit( program_t *prog, double value )
{
	vminstruction_t *p;

	p = prog_emit_instr( prog, vmop_push_lit );
	if( !p ) {
		return NULL;
	}

	p->operand.value = value;
	return p;
}
vminstruction_t *prog_emit_neg( program_t *prog )
{
	return prog_emit_instr( prog, vmop_neg );
}
vminstruction_t *prog_emit_add( program_t *prog )
{
	return prog_emit_instr( prog, vmop_add );
}
vminstruction_t *prog_emit_sub( program_t *prog )
{
	return prog_emit_instr( prog, vmop_sub );
}
vminstruction_t *prog_emit_mul( program_t *prog )
{
	return prog_emit_instr( prog, vmop_mul );
}
vminstruction_t *prog_emit_div( program_t *prog )
{
	return prog_emit_instr( prog, vmop_div );
}
vminstruction_t *prog_emit_pow( program_t *prog )
{
	return prog_emit_instr( prog, vmop_pow );
}
vminstruction_t *prog_emit_mod( program_t *prog )
{
	return prog_emit_instr( prog, vmop_mod );
}
vminstruction_t *prog_emit_call( program_t *prog, unsigned int index )
{
	vminstruction_t *p;

	p = prog_emit_instr( prog, vmop_call );
	if( !p ) {
		return NULL;
	}

	p->operand.slot = index;
	return p;
}

static int expr__literal_semant_f( expr_t *expr, program_t *prog )
{
	expr_literal_t *exprlit;

	assert( expr != NULL );
	assert( prog != NULL );

	exprlit = ( expr_literal_t * )( expr + 1 );
	assert( exprlit->tok != NULL );

	/* Literals are always valid */
	( void )exprlit;
	return 1;
}
static int expr__literal_codegen_f( expr_t *expr, program_t *prog )
{
	expr_literal_t *exprlit;

	assert( expr != NULL );
	assert( prog != NULL );

	exprlit = ( expr_literal_t * )( expr + 1 );
	assert( exprlit->tok != NULL );

	if( prog->fp_asm != NULL ) {
		fprintf( prog->fp_asm, "\tPUSHLIT %g\n", exprlit->tok->number );
	}
	if( !prog_emit_push_lit( prog, exprlit->tok->number ) ) {
		return 0;
	}
	return 1;
}

static int expr__varname_semant_f( expr_t *expr, program_t *prog )
{
	expr_varname_t *exprvar;

	assert( expr != NULL );
	assert( prog != NULL );

	exprvar = ( expr_varname_t * )( expr + 1 );
	assert( exprvar->vartok != NULL );

	exprvar->semanted__decl = prog_find_var_tok( prog, exprvar->vartok );
	if( !exprvar->semanted__decl ) {
		tok_error( exprvar->vartok, va( "Could not find variable \"%.*s\"",
			( size_t )( exprvar->vartok->name_e - exprvar->vartok->name_s ), exprvar->vartok->name_s ) );
		return 0;
	}

	return 1;
}
static int expr__varname_codegen_f( expr_t *expr, program_t *prog )
{
	expr_varname_t *exprvar;

	assert( expr != NULL );
	assert( prog != NULL );

	exprvar = ( expr_varname_t * )( expr + 1 );
	assert( exprvar->vartok != NULL );
	assert( exprvar->semanted__decl != NULL );
	assert( exprvar->semanted__decl->decltok != NULL );

	if( prog->fp_asm != NULL ) {
		fprintf( prog->fp_asm, "\tPUSHVAR %u ;; %.*s\n", exprvar->semanted__decl->vm_index,
			( size_t )( exprvar->vartok->name_e - exprvar->vartok->name_s ), exprvar->vartok->name_s );
	}
	if( !prog_emit_push_var( prog, exprvar->semanted__decl->vm_index ) ) {
		return 0;
	}
	return 1;
}

static int expr__callfunc_semant_f( expr_t *expr, program_t *prog )
{
	expr_callfunc_t *exprfn;
	unsigned int n;

	assert( expr != NULL );
	assert( prog != NULL );

	exprfn = ( expr_callfunc_t * )( expr + 1 );
	assert( exprfn->fntok != NULL );

	exprfn->semanted__decl = find_vmfunc_tok( exprfn->fntok );
	if( !exprfn->semanted__decl ) {
		tok_error( exprfn->fntok, va( "No such function \"%.*s\"",
			( size_t )( exprfn->fntok->name_e - exprfn->fntok->name_s ), exprfn->fntok->name_s ) );
		return 0;
	}

	n = 0;
	while( n < countof( exprfn->parms ) ) {
		if( !exprfn->parms[ n ] ) {
			break;
		}

		if( exprfn->parms[ n ]->pfn_semant != NULL ) {
			if( !exprfn->parms[ n ]->pfn_semant( exprfn->parms[ n ], prog ) ) {
				tok_error( exprfn->fntok, va( "Semantic error in parameter %u of call to \"%.*s\"",
					n + 1, ( size_t )( exprfn->fntok->name_e - exprfn->fntok->name_s ), exprfn->fntok->name_s ) );
				return 0;
			}
		}

		++n;
	}

	if( n < exprfn->semanted__decl->num_parms ) {
		tok_error( exprfn->fntok, va( "Too few arguments to \"%.*s\"; takes %u argument%s, got %u",
			( size_t )( exprfn->fntok->name_e - exprfn->fntok->name_s ), exprfn->fntok->name_s,
			exprfn->semanted__decl->num_parms, exprfn->semanted__decl->num_parms == 1 ? "" : "s", n ) );
		return 0;
	}
	if( n > exprfn->semanted__decl->num_parms ) {
		tok_error( exprfn->fntok, va( "Too many arguments to \"%.*s\"; takes %u argument%s, got %u",
			( size_t )( exprfn->fntok->name_e - exprfn->fntok->name_s ), exprfn->fntok->name_s,
			exprfn->semanted__decl->num_parms, exprfn->semanted__decl->num_parms == 1 ? "" : "s", n ) );
		return 0;
	}

	return 1;
}
static int expr__callfunc_codegen_f( expr_t *expr, program_t *prog )
{
	expr_callfunc_t *exprfn;
	unsigned int i;

	assert( expr != NULL );
	assert( prog != NULL );

	exprfn = ( expr_callfunc_t * )( expr + 1 );
	assert( exprfn->fntok != NULL );
	assert( exprfn->semanted__decl != NULL );

	i = 0;
	while( i < countof( exprfn->parms ) ) {
		if( !exprfn->parms[ i ] ) {
			break;
		}

		if( !exprfn->parms[ i ]->pfn_codegen ) {
			continue;
		}
		if( !exprfn->parms[ i ]->pfn_codegen( exprfn->parms[ i ], prog ) ) {
			return 0;
		}

		++i;
	}

	if( prog->fp_asm != NULL ) {
		fprintf( prog->fp_asm, "\tCALL %u ;; Function %.*s\n", ( unsigned int )exprfn->semanted__decl->intfunc,
			( size_t )( exprfn->fntok->name_e - exprfn->fntok->name_s ), exprfn->fntok->name_s );
	}
	if( !prog_emit_call( prog, ( unsigned int )exprfn->semanted__decl->intfunc ) ) {
		return 0;
	}
	return 1;
}

static int expr__unaryop_semant_f( expr_t *expr, program_t *prog )
{
	expr_unaryop_t *exprun;

	assert( expr != NULL );
	assert( prog != NULL );

	exprun = ( expr_unaryop_t * )( expr + 1 );
	assert( exprun->op != unary_none );
	assert( exprun->subexpr != NULL );

	if( exprun->subexpr->pfn_semant != NULL ) {
		if( !exprun->subexpr->pfn_semant( exprun->subexpr, prog ) ) {
			return 0;
		}
	}

	return 1;
}
static int expr__unaryop_codegen_f( expr_t *expr, program_t *prog )
{
	expr_unaryop_t *exprun;

	assert( expr != NULL );
	assert( prog != NULL );

	exprun = ( expr_unaryop_t * )( expr + 1 );
	assert( exprun->subexpr != NULL );

	if( exprun->subexpr->pfn_codegen != NULL ) {
		if( !exprun->subexpr->pfn_codegen( exprun->subexpr, prog ) ) {
			return 0;
		}
	}

	switch( exprun->op ) {
	case unary_none:
		assert( 0 && "exprun->op is unary_none" );
		return 0;

	case unary_negate:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tNEG\n" );
		}
		if( !prog_emit_neg( prog ) ) {
			return 0;
		}
		break;

	default:
		assert( 0 && "exprun->op is an unhandled unary operator" );
		return 0;
	}

	return 1;
}

static int expr__binop_semant_f( expr_t *expr, program_t *prog )
{
	expr_binaryop_t *exprbin;

	assert( expr != NULL );
	assert( prog != NULL );

	exprbin = ( expr_binaryop_t * )( expr + 1 );
	assert( exprbin->op != binop_none );
	assert( exprbin->left != NULL );
	assert( exprbin->right != NULL );

	if( exprbin->left->pfn_semant != NULL ) {
		if( !exprbin->left->pfn_semant( exprbin->left, prog ) ) {
			return 0;
		}
	}
	if( exprbin->right->pfn_semant != NULL ) {
		if( !exprbin->right->pfn_semant( exprbin->right, prog ) ) {
			return 0;
		}
	}

	return 1;
}
static int expr__binop_codegen_f( expr_t *expr, program_t *prog )
{
	expr_binaryop_t *exprbin;

	assert( expr != NULL );
	assert( prog != NULL );

	exprbin = ( expr_binaryop_t * )( expr + 1 );
	assert( exprbin->left != NULL );
	assert( exprbin->right != NULL );

	if( exprbin->left->pfn_codegen != NULL ) {
		if( !exprbin->left->pfn_codegen( exprbin->left, prog ) ) {
			return 0;
		}
	}
	if( exprbin->right->pfn_codegen != NULL ) {
		if( !exprbin->right->pfn_codegen( exprbin->right, prog ) ) {
			return 0;
		}
	}

	switch( exprbin->op ) {
	case binop_none:
		assert( 0 && "exprbin->op is binop_none" );
		return 0;

	case binop_add:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tADD\n" );
		}
		if( !prog_emit_add( prog ) ) {
			return 0;
		}
		break;
	case binop_sub:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tSUB\n" );
		}
		if( !prog_emit_sub( prog ) ) {
			return 0;
		}
		break;
	case binop_mul:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tMUL\n" );
		}
		if( !prog_emit_mul( prog ) ) {
			return 0;
		}
		break;
	case binop_div:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tDIV\n" );
		}
		if( !prog_emit_div( prog ) ) {
			return 0;
		}
		break;
	case binop_mod:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tMOD\n" );
		}
		if( !prog_emit_mod( prog ) ) {
			return 0;
		}
		break;
	case binop_pow:
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tPOW\n" );
		}
		if( !prog_emit_pow( prog ) ) {
			return 0;
		}
		break;

	default:
		assert( 0 && "exprbin->op is an unhandled value" );
		return 0;
	}

	return 1;
}

static int stmt__return_semant_f( stmt_t *stmt, program_t *prog )
{
	stmt_return_t *stmtret;

	assert( stmt != NULL );
	assert( prog != NULL );

	stmtret = ( stmt_return_t * )( stmt + 1 );
	assert( stmtret->retexpr != NULL );

	if( stmtret->retexpr->pfn_semant != NULL ) {
		if( !stmtret->retexpr->pfn_semant( stmtret->retexpr, prog ) ) {
			return 0;
		}
	}

	return 1;
}
static int stmt__return_codegen_f( stmt_t *stmt, program_t *prog )
{
	stmt_return_t *stmtret;

	assert( stmt != NULL );
	assert( prog != NULL );

	stmtret = ( stmt_return_t * )( stmt + 1 );
	assert( stmtret->retexpr != NULL );

	if( stmtret->retexpr->pfn_codegen != NULL ) {
		if( !stmtret->retexpr->pfn_codegen( stmtret->retexpr, prog ) ) {
			return 0;
		}
	}

	if( prog->fp_asm != NULL ) {
		fprintf( prog->fp_asm, "\tRETURN\n" );
	}
	if( !prog_emit_return( prog ) ) {
		return 0;
	}
	return 1;
}

static int stmt__vardecl_semant_f( stmt_t *stmt, program_t *prog )
{
	stmt_vardecl_t *stmtvar;
	vardecl_t *v;

	assert( stmt != NULL );
	assert( prog != NULL );

	stmtvar = ( stmt_vardecl_t * )( stmt + 1 );
	assert( stmtvar->vartok != NULL );

	v = prog_find_var_tok( prog, stmtvar->vartok );
	if( v != NULL ) {
		tok_error( stmtvar->vartok, va( "Variable is already declared: \"%.*s\"",
			( size_t )( stmtvar->vartok->name_e - stmtvar->vartok->name_s ), stmtvar->vartok->name_s ) );
		return 0;
	}

	assert( ( !stmtvar->is_const || stmtvar->defexpr != NULL ) && "Constants require a default expression" );
	if( stmtvar->defexpr != NULL && stmtvar->defexpr->pfn_semant != NULL ) {
		if( !stmtvar->defexpr->pfn_semant( stmtvar->defexpr, prog ) ) {
			return 0;
		}
	}

	stmtvar->semanted__decl = ( vardecl_t * )mem_alloc( sizeof( vardecl_t ) );
	if( !stmtvar->semanted__decl ) {
		tok_error( stmtvar->vartok, "Out of memory" );
		return 0;
	}

	mem_attach( ( void * )stmtvar->semanted__decl, ( void * )stmt );

	stmtvar->semanted__decl->decltok = stmtvar->vartok;
	stmtvar->semanted__decl->is_const = stmtvar->is_const;
	stmtvar->semanted__decl->vm_index = prog->vm_indexes++;

	stmtvar->semanted__decl->v_prev = prog->v_tail;
	stmtvar->semanted__decl->v_next = NULL;
	if( stmtvar->semanted__decl->v_prev != NULL ) {
		stmtvar->semanted__decl->v_prev->v_next = stmtvar->semanted__decl;
	} else {
		prog->v_head = stmtvar->semanted__decl;
	}
	prog->v_tail = stmtvar->semanted__decl;

	return 1;
}
static int stmt__vardecl_codegen_f( stmt_t *stmt, program_t *prog )
{
	stmt_vardecl_t *stmtvar;

	assert( stmt != NULL );
	assert( prog != NULL );

	stmtvar = ( stmt_vardecl_t * )( stmt + 1 );
	assert( stmtvar->vartok != NULL );
	assert( stmtvar->semanted__decl != NULL );
	assert( !stmtvar->is_const || stmtvar->defexpr != NULL );

	if( stmtvar->defexpr != NULL && stmtvar->defexpr->pfn_codegen != NULL ) {
		if( !stmtvar->defexpr->pfn_codegen( stmtvar->defexpr, prog ) ) {
			return 0;
		}
	}

	if( stmtvar->is_const ) {
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tVAR_CONST %u", stmtvar->semanted__decl->vm_index );
		}
		if( !prog_emit_var_const( prog, stmtvar->semanted__decl->vm_index  ) ) {
			return 0;
		}
	} else {
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tVAR_DECL %u", stmtvar->semanted__decl->vm_index );
		}
		if( !prog_emit_var_decl( prog, stmtvar->semanted__decl->vm_index ) ) {
			return 0;
		}
	}

	if( prog->fp_asm != NULL ) {
		fprintf( prog->fp_asm, " ;; %.*s\n",
			( size_t )( stmtvar->vartok->name_e - stmtvar->vartok->name_s ), stmtvar->vartok->name_s );
	}

	if( stmtvar->defexpr != NULL ) {
		if( prog->fp_asm != NULL ) {
			printf( "\tSTORE %u\n", stmtvar->semanted__decl->vm_index );
		}
		if( !prog_emit_store( prog, stmtvar->semanted__decl->vm_index ) ) {
			return 0;
		}
	}

	return 1;
}

static int stmt__varassign_semant_f( stmt_t *stmt, program_t *prog )
{
	stmt_varassign_t *stmtset;

	assert( stmt != NULL );
	assert( prog != NULL );

	stmtset = ( stmt_varassign_t * )( stmt + 1 );
	assert( stmtset->vartok != NULL );
	assert( stmtset->defexpr != NULL );

	stmtset->semanted__decl = prog_find_var_tok( prog, stmtset->vartok );
	if( !stmtset->semanted__decl ) {
		tok_error( stmtset->vartok, va( "Undeclared variable \"%.*s\"",
			( size_t )( stmtset->vartok->name_e - stmtset->vartok->name_s ), stmtset->vartok->name_s ) );
		return 0;
	}

	if( stmtset->semanted__decl->is_const ) {
		tok_error( stmtset->vartok, va( "Cannot write to read-only variable \"%.*s\"",
			( size_t )( stmtset->vartok->name_e - stmtset->vartok->name_s ), stmtset->vartok->name_s ) );
		return 0;
	}

	if( stmtset->defexpr->pfn_semant != NULL ) {
		if( !stmtset->defexpr->pfn_semant( stmtset->defexpr, prog ) ) {
			return 0;
		}
	}

	return 1;
}
static int stmt__varassign_codegen_f( stmt_t *stmt, program_t *prog )
{
	stmt_varassign_t *stmtset;

	assert( stmt != NULL );
	assert( prog != NULL );

	stmtset = ( stmt_varassign_t * )( stmt + 1 );
	assert( stmtset->vartok != NULL );
	assert( stmtset->defexpr != NULL );
	assert( stmtset->semanted__decl != NULL );
	assert( stmtset->semanted__decl->is_const == 0 );

	if( stmtset->defexpr->pfn_codegen != NULL ) {
		if( !stmtset->defexpr->pfn_codegen( stmtset->defexpr, prog ) ) {
			return 0;
		}
	}

	if( stmtset->compoundop != binop_none ) {
		if( prog->fp_asm != NULL ) {
			fprintf( prog->fp_asm, "\tPUSHVAR %u ;; %.*s\n",
				stmtset->semanted__decl->vm_index,
				( size_t )( stmtset->vartok->name_e - stmtset->vartok->name_s ), stmtset->vartok->name_s );
		}
		if( !prog_emit_push_var( prog, stmtset->semanted__decl->vm_index ) ) {
			return 0;
		}

		switch( stmtset->compoundop ) {
		case binop_add:
			if( prog->fp_asm != NULL ) {
				fprintf( prog->fp_asm, "\tADD\n" );
			}
			if( !prog_emit_add( prog ) ) {
				return 0;
			}
			break;
		case binop_sub:
			if( prog->fp_asm != NULL ) {
				fprintf( prog->fp_asm, "\tSUB\n" );
			}
			if( !prog_emit_sub( prog ) ) {
				return 0;
			}
			break;
		case binop_mul:
			if( prog->fp_asm != NULL ) {
				fprintf( prog->fp_asm, "\tMUL\n" );
			}
			if( !prog_emit_mul( prog ) ) {
				return 0;
			}
			break;
		case binop_div:
			if( prog->fp_asm != NULL ) {
				fprintf( prog->fp_asm, "\tDIV\n" );
			}
			if( !prog_emit_div( prog ) ) {
				return 0;
			}
			break;
		case binop_mod:
			if( prog->fp_asm != NULL ) {
				fprintf( prog->fp_asm, "\tMOD\n" );
			}
			if( !prog_emit_mod( prog ) ) {
				return 0;
			}
			break;
		case binop_pow:
			if( prog->fp_asm != NULL ) {
				fprintf( prog->fp_asm, "\tPOW\n" );
			}
			if( !prog_emit_pow( prog ) ) {
				return 0;
			}
			break;
		default:
			assert( 0 && "Unhandled binary operator for compound assignment" );
			return 0;
		}
	}

	if( prog->fp_asm != NULL ) {
		fprintf( prog->fp_asm, "\tSTORE %u ;; %.*s\n",
			stmtset->semanted__decl->vm_index,
			( size_t )( stmtset->vartok->name_e - stmtset->vartok->name_s ), stmtset->vartok->name_s );
	}
	if( !prog_emit_store( prog, stmtset->semanted__decl->vm_index ) ) {
		return 0;
	}
	return 1;
}

source_t *src_new( const char *filename )
{
	source_t *src;

	src = ( source_t * )mem_alloc( sizeof( *src ) );
	if( !src ) {
		return NULL;
	}

	src->filename = mem_strdup( filename );
	mem_attach( ( void * )src->filename, ( void * )src );

	return src;
}
void src_settext( source_t *src, const char *text )
{
	assert( src != NULL );

	mem_free( ( void * )src->text );
	src->text = mem_strdup( text );
	src->p = src->text;
	if( !src->text ) {
		return;
	}

	mem_attach( ( void * )src->text, ( void * )src );
}
source_t *src_open( const char *filename )
{
	source_t *src;
	FILE *fp;
	size_t n;

	src = src_new( filename );

	fp = fopen( filename, "rb" );
	if( !fp ) {
		mem_free( ( void * )src );

		fprintf( stderr, "[%s] ERROR: fopen() failed\n", filename );
		return NULL;
	}

	fseek( fp, 0, SEEK_END );
	n = ( size_t )ftell( fp );

	fseek( fp, 0, SEEK_SET );

	src->text = ( char * )mem_alloc( n + 1 );
	if( !src->text ) {
		mem_free( ( void * )src );
		return NULL;
	}

	mem_attach( ( void * )src->text, ( void * )src );

	if( !fread( ( void * )src->text, n, 1, fp ) ) {
		fclose( fp );
		fp = NULL;

		mem_free( ( void * )src );

		fprintf( stderr, "[%s] ERROR: fread() failed\n", filename );
		return NULL;
	}

	fclose( fp );
	fp = NULL;

	src->text[ n ] = '\0';
	src->p = src->text;

	return src;
}
source_t *src_delete( source_t *src )
{
	return ( source_t * )mem_free( ( void * )src );
}

const char *toktype_to_string( toktype_t tt )
{
	switch( tt )
	{
	case tok_eof:		return "EOF";

	case tok_name:		return "name";
	case tok_number:	return "number";

	case tok_lparen:	return "'('";
	case tok_rparen:	return "')'";
	case tok_add:		return "'+'";
	case tok_sub:		return "'-'";
	case tok_mul:		return "'*'";
	case tok_div:		return "'/'";
	case tok_mod:		return "'%'";
	case tok_pow:		return "'^'";
	case tok_semicolon:	return "';'";
	case tok_comma:		return "','";
	case tok_eq:		return "'='";
	}

	assert( 0 && "Unreachable (Unhandled toktype_t)" );
	return "(unknown)";
}

void src_error_p( const source_t *src, const char *p, const char *errormsg )
{
	unsigned int line;
	unsigned int column;
	const char *q;

	assert( src != NULL );
	assert( p != NULL );
	assert( errormsg != NULL );

	assert( src->filename != NULL );
	assert( src->text != NULL );

	line = 1;
	q = src->text;
	for(;;) {
		const char *r;

		r = strchr( q, '\n' );
		if( !r || r > p ) {
			break;
		}

		++line;
		q = r + 1;
	}

	column = ( unsigned int )( p - q ) + 1;

	fprintf( stderr, "[%s(%u:%u)] ERROR: %s\n", src->filename, line, column, errormsg );
}
void src_error( const source_t *src, const char *errormsg )
{
	src_error_p( src, src->p, errormsg );
}
void tok_error( const token_t *tok, const char *errormsg )
{
	assert( tok != NULL );
	assert( tok->source != NULL );
	assert( tok->name_s != NULL );

	src_error_p( tok->source, tok->name_s, errormsg );
}

static int src__is_name_start_( char ch )
{
	return ( ch>='a' && ch<='z' ) || ( ch>='A' && ch<='Z' ) || ( ch=='_' );
}
static int src__is_digit_( char ch )
{
	return ( ch>='0' && ch<='9' );
}
static int src__is_name_( char ch )
{
	return src__is_name_start_( ch ) || src__is_digit_( ch );
}

static const char *src__skip_whitespace_( const char *p )
{
	int retry;
	do {
		retry = 0;

		while( *( const unsigned char * )p <= ' ' && *p != '\0' ) {
			++p;
		}

		if( *p == '/' && *( p + 1 ) == '/' ) {
			const char *q;

			q = strchr( p + 2, '\n' );
			if( !q ) {
				p = strchr( p + 2, '\0' );
			} else {
				p = q + 1;
			}

			retry = 1;
			continue;
		}

		if( *p == '/' && *( p + 1 ) == '*' ) {
			const char *q;

			q = strstr( p + 2, "*/" );
			if( !q ) {
				p = strchr( p + 2, '\0' );
			} else {
				p = q + 2;
			}

			retry = 1;
			continue;
		}
	} while( retry && *p != '\0' );

	return p;
}
static const char *src__skip_name_( const char *p )
{
	if( !src__is_name_start_( *p ) ) {
		return p;
	}

	do {
		++p;
	} while( src__is_name_( *p ) );

	return p;
}
static const char *src__skip_number_( const char *p, double *out_number )
{
	const char *q;
	unsigned int whole;
	unsigned int fract, fractmag, tmpfract;
	unsigned int exp;
	int expsign;
	double f;

	whole = 0;
	fract = 0;
	exp = 0;
	expsign = 1;

	q = p;
	while( src__is_digit_( *p ) ) {
		whole *= 10;
		whole += ( unsigned int )( *p - '0' );
		++p;
	}

	if( *p == '.' ) {
		if( q == p && !src__is_digit_( *( p + 1 ) ) ) {
			return q;
		}

		++p;
		while( src__is_digit_( *p ) ) {
			fract *= 10;
			fract += ( unsigned int )( *p - '0' );
			++p;
		}
	}

	if( *p == 'e' || *p == 'E' ) {
		++p;
		if( *p == '-' ) {
			expsign = -1;
			++p;
		} else if( *p == '+' ) {
			++p;
		}
		while( src__is_digit_( *p ) ) {
			exp *= 10;
			exp += ( unsigned int )( *p - '0' );
			++p;
		}
	}

	f = ( double )whole;

	fractmag = 1;
	tmpfract = fract;
	while( tmpfract > 0 ) {
		fractmag *= 10;
		tmpfract /= 10;
	}

	f += ( ( double )fract )/( double )fractmag;
	if( expsign > 0 ) {
		while( exp > 0 ) {
			f *= 10;
			--exp;
		}
	} else {
		while( exp > 0 ) {
			f /= 10;
			--exp;
		}
	}

	*out_number = f;

	return p;
}

token_t *src_lex( source_t *src )
{
	const char *p;
	token_t *tok;

	assert( src != NULL );

	if( src->t_unread != NULL ) {
		tok = src->t_unread;
		src->t_unread = src->t_unread->t_next;

		return tok;
	}

	if( src->t_tail != NULL && src->t_tail->type == tok_eof ) {
		return src->t_tail;
	}

	tok = ( token_t * )mem_alloc( sizeof( *tok ) );
	if( !tok ) {
		src_error( src, "Out of memory" );
		return NULL;
	}

	src->p = src__skip_whitespace_( src->p );
	p = src->p;

	p = src__skip_name_( p );
	if( p != src->p ) {
		tok->type = tok_name;
		goto L_init_tok;
	}

	p = src__skip_number_( p, &tok->number );
	if( p != src->p ) {
		tok->type = tok_number;
		goto L_init_tok;
	}

	if( strchr( "=()+-*/%^;,", *p ) != NULL ) {
		tok->type = ( toktype_t )*p;
		++p;
		goto L_init_tok;
	}

	src_error( src, va( "Unexpected character '%c' (0x%.2X)", *p, ( unsigned int )*p ) );
	return ( token_t * )mem_free( ( void * )tok );

L_init_tok:
	tok->name_s = src->p;
	tok->name_e = p;
	src->p = p;

	tok->source = src;
	tok->t_prev = src->t_tail;
	tok->t_next = NULL;
	if( tok->t_prev != NULL ) {
		tok->t_prev->t_next = tok;
	} else {
		src->t_head = tok;
	}
	src->t_tail = tok;

#ifdef DEBUGPRINT_LEXER
	printf( " -- tok %s <%.*s> :: %g\n",
		toktype_to_string( tok->type ), ( size_t )( tok->name_e - tok->name_s ), tok->name_s, tok->number );
#endif

	return tok;
}

void src_unlex( source_t *src )
{
	assert( src != NULL );

	if( src->t_unread != NULL ) {
		if( src->t_unread->t_prev != NULL ) {
			src->t_unread = src->t_unread->t_prev;
		}
	} else {
		src->t_unread = src->t_tail;
	}
}

token_t *src_check( source_t *src, toktype_t tt )
{
	token_t *t;

	assert( src != NULL );

	t = src_lex( src );
	if( !t ) {
		return NULL;
	}

	if( t->type != tt ) {
		src_unlex( src );
		return NULL;
	}

	return t;
}
token_t *src_expect( source_t *src, toktype_t tt )
{
	token_t *t;

	assert( src != NULL );

	t = src_lex( src );
	if( !t ) {
		src_error( src, va( "Expected %s, but got nothing", toktype_to_string( tt ) ) );
		return NULL;
	}

	if( t->type != tt ) {
		if( t->type == tok_name || t->type == tok_number ) {
			tok_error( t, va( "Expected %s, but got %s \"%.*s\"",
				toktype_to_string( tt ), toktype_to_string( t->type ),
				( size_t )( t->name_e - t->name_s ), t->name_s ) );
		} else {
			tok_error( t, va( "Expected %s, but got %s",
				toktype_to_string( tt ), toktype_to_string( t->type ) ) );
		}
		src_unlex( src );
		return NULL;
	}

	return t;
}

program_t *prog_new( source_t *src )
{
	static const char *builtin_consts[] = {
		"w", "t", "T", "pi", "phase"
	};
	program_t *prog;
	size_t i;

	assert( src != NULL );

	prog = ( program_t * )mem_alloc( sizeof( *prog ) );
	if( !prog ) {
		src_error( src, "Out of memory" );
		return NULL;
	}

	mem_attach( ( void * )prog, ( void * )src );

	prog->source = src;

	for( i = 0; i < countof( builtin_consts ); ++i ) {
		vardecl_t *v;
		token_t *t;

		t = ( token_t * )mem_alloc( sizeof( *t ) );
		if( !t ) {
			src_error( src, "Out of memory" );
			return ( program_t * )mem_free( ( void * )prog );
		}

		mem_attach( ( void * )t, ( void * )prog );

		t->name_s = builtin_consts[ i ];
		t->name_e = strchr( t->name_s, '\0' );

		v = ( vardecl_t * )mem_alloc( sizeof( *v ) );
		if( !v ) {
			src_error( src, "Out of memory" );
			return ( program_t * )mem_free( ( void * )prog );
		}

		mem_attach( ( void * )v, ( void * )prog );

		v->decltok = t;
		v->is_const = 1;
		v->vm_index = ( unsigned int )i;

		v->v_prev = prog->v_tail;
		v->v_next = NULL;
		if( v->v_prev != NULL ) {
			v->v_prev->v_next = v;
		} else {
			prog->v_head = v;
		}
		prog->v_tail = v;
	}

	prog->vm_indexes = i;

	return prog;
}

stmt_t *stmt_new( program_t *prog, size_t n )
{
	stmt_t *stmt;

	assert( prog != NULL );
	assert( prog->source != NULL );

	stmt = ( stmt_t * )mem_alloc( sizeof( *stmt ) + n );
	if( !stmt ) {
		src_error( prog->source, "Out of memory" );
		return NULL;
	}

	mem_attach( ( void * )stmt, ( void * )prog );

	stmt->s_prev = prog->s_tail;
	stmt->s_next = NULL;
	if( stmt->s_prev != NULL ) {
		stmt->s_prev->s_next = stmt;
	} else {
		prog->s_head = stmt;
	}
	prog->s_tail = stmt;

	return stmt;
}
expr_t *expr_new( program_t *prog, stmt_t *stmt, size_t n )
{
	expr_t *expr;

	assert( prog != NULL );
	assert( prog->source != NULL );
	assert( stmt != NULL );

	expr = ( expr_t * )mem_alloc( sizeof( *expr ) + n );
	if( !expr ) {
		src_error( prog->source, "Out of memory" );
		return NULL;
	}

	mem_attach( ( void * )expr, ( void * )stmt );

	return expr;
}

unaryop_t toktype_to_unaryop( toktype_t tt )
{
	if( tt == tok_sub ) {
		return unary_negate;
	}

	return unary_none;
}
binaryop_t toktype_to_binaryop( toktype_t tt )
{
	if( tt == tok_add ) {
		return binop_add;
	} else if( tt == tok_sub ) {
		return binop_sub;
	} else if( tt == tok_mul ) {
		return binop_mul;
	} else if( tt == tok_div ) {
		return binop_div;
	} else if( tt == tok_mod ) {
		return binop_mod;
	} else if( tt == tok_pow ) {
		return binop_pow;
	}

	return binop_none;
}

expr_t *prog_parse_subexpr( program_t *prog, stmt_t *stmt, unsigned char precedence );
expr_t *prog_parse_expr( program_t *prog, stmt_t *stmt );

static expr_t *prog_parse_expr__terminal_( program_t *prog, stmt_t *stmt )
{
	token_t *tok;
	expr_t *expr;

	tok = src_lex( prog->source );
	if( !tok ) {
		return NULL;
	}

	if( tok->type == tok_lparen ) {
		expr = prog_parse_expr( prog, stmt );
		if( !expr ) {
			return NULL;
		}

		if( !src_expect( prog->source, tok_rparen ) ) {
			return ( expr_t * )mem_free( ( void * )expr );
		}

		return expr;
	}

	if( tok->type == tok_number ) {
		expr_literal_t *exprlit;

		expr = expr_new( prog, stmt, sizeof( *exprlit ) );
		if( !expr ) {
			return NULL;
		}

		expr->pfn_semant = &expr__literal_semant_f;
		expr->pfn_codegen = &expr__literal_codegen_f;

		exprlit = ( expr_literal_t * )( expr + 1 );

		exprlit->tok = tok;
		return expr;
	}

	if( tok->type == tok_name ) {
		token_t *parmtok;

		parmtok = src_check( prog->source, tok_lparen );
#ifdef DEBUGPRINT_PARSER
		printf( " ## parmtok: %p -- for tok <%.*s>\n", ( void * )parmtok, ( size_t )( tok->name_e - tok->name_s ), tok->name_s );
#endif

		if( parmtok != NULL ) {
			expr_callfunc_t *exprfunc;
			size_t i;

			expr = expr_new( prog, stmt, sizeof( *exprfunc ) );
			if( !expr ) {
				return NULL;
			}

			expr->pfn_semant = &expr__callfunc_semant_f;
			expr->pfn_codegen = &expr__callfunc_codegen_f;

			exprfunc = ( expr_callfunc_t * )( expr + 1 );
			exprfunc->fntok = tok;

			if( !src_check( prog->source, tok_rparen ) ) {
				i = 0;
				for(;;) {
					if( i == countof( exprfunc->parms ) ) {
						src_error( prog->source, va( "Too many parameters to function: %.*s",
							( size_t )( exprfunc->fntok->name_e - exprfunc->fntok->name_s ),
							exprfunc->fntok->name_s ) );
						return ( expr_t * )mem_free( ( void * )expr );
					}

					exprfunc->parms[ i ] = prog_parse_expr( prog, stmt );
					if( !exprfunc->parms[ i ] ) {
						return ( expr_t * )mem_free( ( void * )expr );
					}

					++i;

					if( !src_check( prog->source, tok_comma ) ) {
						if( !src_expect( prog->source, tok_rparen ) ) {
							return ( expr_t * )mem_free( ( void * )expr );
						}

						break;
					}
				}
			}
		} else {
			expr_varname_t *exprvar;

			expr = expr_new( prog, stmt, sizeof( *exprvar ) );
			if( !expr ) {
				return NULL;
			}

			expr->pfn_semant = &expr__varname_semant_f;
			expr->pfn_codegen = &expr__varname_codegen_f;

			exprvar = ( expr_varname_t * )( expr + 1 );
			exprvar->vartok = tok;
		}

		return expr;
	}

	tok_error( tok, va( "Expected name or number for expression; got \"%.*s\"",
		( size_t )( tok->name_e - tok->name_s ), tok->name_s ) );
	return NULL;
}

static expr_t *prog_parse_expr__unaryop_( program_t *prog, stmt_t *stmt )
{
	token_t *tok;
	expr_t *expr;
	expr_unaryop_t *exprun;

	tok = src_check( prog->source, tok_sub );
	if( !tok ) {
		return prog_parse_expr__terminal_( prog, stmt );
	}

	expr = expr_new( prog, stmt, sizeof( *exprun ) );
	if( !expr ) {
		return NULL;
	}

	expr->pfn_semant = &expr__unaryop_semant_f;
	expr->pfn_codegen = &expr__unaryop_codegen_f;

	exprun = ( expr_unaryop_t * )( expr + 1 );

	exprun->op = unary_negate;

	exprun->subexpr = prog_parse_subexpr( prog, stmt, g_unaryop_precedence );
	if( !exprun->subexpr ) {
		return NULL;
	}

	return expr;
}
expr_t *prog_parse_subexpr( program_t *prog, stmt_t *stmt, unsigned char precedence )
{
	token_t *t;
	expr_t *tree;
	expr_t *right;
	expr_t *expr;
	expr_binaryop_t *exprbinop;
	size_t i;

	tree = prog_parse_expr__unaryop_( prog, stmt );
	if( !tree ) {
		return NULL;
	}

	for(;;) {
		const operator_t *op;

		t = src_lex( prog->source );
		if( !t ) {
			return ( expr_t * )mem_free( ( void * )tree );
		}

		if( toktype_to_binaryop( t->type ) == binop_none ) {
			src_unlex( prog->source );
			break;
		}

		op = NULL;
		for( i=0; i<countof( g_operators ); ++i ) {
			if( g_operators[i].precedence < precedence ) {
				continue;
			}

			if( t->type == g_operators[i].tok ) {
				op = &g_operators[i];
				break;
			}
		}

		if( !op ) {
			src_unlex( prog->source );
			break;
		}

		right = prog_parse_subexpr( prog, stmt, op->precedence + 1 );
		if( !right ) {
			return ( expr_t * )mem_free( ( void * )tree );
		}

		expr = expr_new( prog, stmt, sizeof( *exprbinop ) );
		if( !expr ) {
			return ( expr_t * )mem_free( ( void * )tree );
		}

		expr->pfn_semant = &expr__binop_semant_f;
		expr->pfn_codegen = &expr__binop_codegen_f;

		exprbinop = ( expr_binaryop_t * )( expr + 1 );

		exprbinop->op = op->binop;
		exprbinop->left = tree;
		exprbinop->right = right;

		tree = expr;
	}

	return tree;
}
expr_t *prog_parse_expr( program_t *prog, stmt_t *stmt )
{
	return prog_parse_subexpr( prog, stmt, 0 );
}

stmt_t *prog_parse_stmt( program_t *prog )
{
	token_t *t, *endt;
	stmt_t *stmt;

	assert( prog != NULL );
	assert( prog->source != NULL );

	t = src_lex( prog->source );
	if( !t ) {
		return NULL;
	}

	if( t->type != tok_name ) {
		src_unlex( prog->source );
		src_expect( prog->source, tok_name );
		return NULL;
	}

	if( tok_cmp( t, "return" ) ) {
		stmt_return_t *retstmt;

#ifdef DEBUGPRINT_PARSER
		printf( " :: return\n" );
#endif

		stmt = stmt_new( prog, sizeof( *retstmt ) );
		if( !stmt ) {
			return NULL;
		}

		stmt->pfn_semant = &stmt__return_semant_f;
		stmt->pfn_codegen = &stmt__return_codegen_f;

		retstmt = ( stmt_return_t * )( stmt + 1 );
		retstmt->retexpr = prog_parse_expr( prog, stmt );
		if( !retstmt->retexpr ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}
	} else if( tok_cmp( t, "var" ) ) {
		stmt_vardecl_t *varstmt;

#ifdef DEBUGPRINT_PARSER
		printf( " :: var\n" );
#endif

		stmt = stmt_new( prog, sizeof( *varstmt ) );
		if( !stmt ) {
			return NULL;
		}

		stmt->pfn_semant = &stmt__vardecl_semant_f;
		stmt->pfn_codegen = &stmt__vardecl_codegen_f;

		varstmt = ( stmt_vardecl_t * )( stmt + 1 );
		varstmt->vartok = src_expect( prog->source, tok_name );
		if( !varstmt->vartok ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}

		if( src_check( prog->source, tok_eq ) != NULL ) {
			varstmt->defexpr = prog_parse_expr( prog, stmt );
			if( !varstmt->defexpr ) {
				return ( stmt_t * )mem_free( ( void * )stmt );
			}
		}
	} else if( tok_cmp( t, "let" ) ) {
		stmt_vardecl_t *varstmt;

#ifdef DEBUGPRINT_PARSER
		printf( " :: let\n" );
#endif

		stmt = stmt_new( prog, sizeof( *varstmt ) );
		if( !stmt ) {
			return NULL;
		}

		stmt->pfn_semant = &stmt__vardecl_semant_f;
		stmt->pfn_codegen = &stmt__vardecl_codegen_f;

		varstmt = ( stmt_vardecl_t * )( stmt + 1 );
		varstmt->vartok = src_expect( prog->source, tok_name );
		if( !varstmt->vartok ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}

		if( !src_expect( prog->source, tok_eq ) ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}

		varstmt->defexpr = prog_parse_expr( prog, stmt );
		if( !varstmt->defexpr ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}

		varstmt->is_const = 1;
	} else {
		stmt_varassign_t *setstmt;
		token_t *t1, *t2;

#ifdef DEBUGPRINT_PARSER
		printf( " :: assign\n" );
#endif

		if( t->type != tok_name ) {
			src_unlex( prog->source );
			src_expect( prog->source, tok_name );
			return NULL;
		}

		t1 = src_lex( prog->source );
		if( !t1 ) {
			return NULL;
		}

		if( t1->type != tok_eq ) {
			if( toktype_to_binaryop( t1->type ) == binop_none ) {
				tok_error( t1, va( "Expected assignment operator, got \"%.*s\"",
					( size_t )( t1->name_e - t1->name_s ), t1->name_s ) );
				return NULL;
			}

			t2 = src_expect( prog->source, tok_eq );
			if( !t2 ) {
				return NULL;
			}
		} else {
			t2 = NULL;
		}

		stmt = stmt_new( prog, sizeof( *setstmt ) );
		if( !stmt ) {
			return NULL;
		}

		stmt->pfn_semant = &stmt__varassign_semant_f;
		stmt->pfn_codegen = &stmt__varassign_codegen_f;

		setstmt = ( stmt_varassign_t * )( stmt + 1 );

		setstmt->vartok = t;
		if( !setstmt->vartok ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}

		setstmt->defexpr = prog_parse_expr( prog, stmt );
		if( !setstmt->defexpr ) {
			return ( stmt_t * )mem_free( ( void * )stmt );
		}

		if( t2 != NULL ) {
			assert( t1 != NULL );

			setstmt->compoundop = toktype_to_binaryop( t1->type );
			assert( setstmt->compoundop != binop_none );
		} else {
			setstmt->compoundop = binop_none;
		}
	}

	assert( stmt!=NULL );

	src_unlex( prog->source );
	t = src_lex( prog->source );
	endt = src_lex( prog->source );

	if( endt != NULL ) {
		const char *nl;

		assert( t != NULL );
		assert( t->name_e != NULL );
		assert( endt->name_s != NULL );

		nl = strchr( t->name_e, '\n' );
		if( nl < endt->name_s ) {
			src_unlex( prog->source );
			return stmt;
		}

		if( endt->type == tok_semicolon || endt->type == tok_eof ) {
			return stmt;
		}

		src_unlex( prog->source );
	}

	if( !src_expect( prog->source, tok_semicolon ) ) {
		mem_free( ( void * )stmt );
		return NULL;
	}

	return stmt;
}

int prog_parse( program_t *prog )
{
	assert( prog != NULL );
	assert( prog->source != NULL );

	for(;;) {
		token_t *t;
		stmt_t *stmt;

		t = src_lex( prog->source );
		if( !t ) {
			return 0;
		}

		if( t->type == tok_eof ) {
			break;
		}

		if( t->type == tok_semicolon ) {
			continue;
		}

		src_unlex( prog->source );

		stmt = prog_parse_stmt( prog );
		if( !stmt ) {
			return 0;
		}

#ifdef DEBUGPRINT_PARSER
		printf( " :: end-stmt ;;\n" );
#endif
	}

	return 1;
}
int prog_semant( program_t *prog )
{
	stmt_t *stmt;

	assert( prog != NULL );

	for( stmt=prog->s_head; stmt!=NULL; stmt=stmt->s_next ) {
		if( !stmt->pfn_semant ) {
			continue;
		}

		if( !stmt->pfn_semant( stmt, prog ) ) {
			return 0;
		}
	}

	return 1;
}
int prog_codegen( program_t *prog )
{
	stmt_t *stmt;

	assert( prog != NULL );

	for( stmt=prog->s_head; stmt!=NULL; stmt=stmt->s_next ) {
		if( !stmt->pfn_codegen ) {
			continue;
		}

		if( !stmt->pfn_codegen( stmt, prog ) ) {
			return 0;
		}
	}

	return 1;
}

program_t *prog_compile( source_t *src, FILE *fpasmlisting )
{
	program_t *prog;

	prog = prog_new( src );
	if( !prog ) {
		mem_free( ( void * )src );
		return NULL;
	}

	prog->fp_asm = fpasmlisting;

	if( !prog_parse( prog ) ) {
		src_error( prog->source, "prog_parse() failed" );
		return ( program_t * )mem_free( ( void * )prog );
	}
	if( !prog_semant( prog ) ) {
		src_error( prog->source, "prog_semant() failed" );
		return ( program_t * )mem_free( ( void * )prog );
	}

	if( fpasmlisting != NULL ) {
		fprintf( fpasmlisting, ";; CODEGEN :: %s\n", src->filename );
	}
	if( !prog_codegen( prog ) ) {
		src_error( prog->source, "prog_codegen() failed" );
		return ( program_t * )mem_free( ( void * )prog );
	}

	return prog;
}

int compilefile( const char *filename )
{
	source_t *src;
	program_t *prog;
	vmcontext_t vm;
	double val;

	src = src_open( filename );
	if( !src ) {
		return 0;
	}

	prog = prog_compile( src, stdout );

	if( prog != NULL ) {
		if( !vminit( &vm, prog->vm_indexes ) ) {
			src_error( src, "vminit() failed" );
			return 0;
		}

		install_vm_funcs( &vm );

		vm.instr_s = prog->instrs;
		vm.instr_e = prog->instrs + prog->num_instrs;


		vm.regs[ 0 ] = 440.0;
		vm.regs[ 1 ] = 0.5;
		vm.regs[ 2 ] = vm.regs[ 1 ];
		vm.regs[ 3 ] = _M_PI;
		vm.regs[ 4 ] = vm.regs[ 1 ]*( 2.0*_M_PI );
		val = vmeval( &vm );
		printf( "vmeval( %g, %g ) = %g\n", vm.regs[ 0 ], vm.regs[ 1 ], val );

		vm.regs[ 0 ] = 440.0;
		vm.regs[ 1 ] = 0.1;
		vm.regs[ 2 ] = vm.regs[ 1 ];
		vm.regs[ 3 ] = _M_PI;
		vm.regs[ 4 ] = vm.regs[ 1 ]*( 2.0*_M_PI );
		val = vmeval( &vm );
		printf( "vmeval( %g, %g ) = %g\n", vm.regs[ 0 ], vm.regs[ 1 ], val );

		vm.regs[ 0 ] = 440.0;
		vm.regs[ 1 ] = 0.9;
		vm.regs[ 2 ] = vm.regs[ 1 ];
		vm.regs[ 3 ] = _M_PI;
		vm.regs[ 4 ] = vm.regs[ 1 ]*( 2.0*_M_PI );
		val = vmeval( &vm );
		printf( "vmeval( %g, %g ) = %g\n", vm.regs[ 0 ], vm.regs[ 1 ], val );

		vmfini( &vm );
	}

	mem_free( ( void * )src );
	return prog != NULL ? 1 : 0;
}


/*
===============================================================================

	SOUND SAMPLER

===============================================================================
*/

#define NOTE_A			0
#define NOTE_A_SHARP	1
#define NOTE_B			2
#define NOTE_C			3
#define NOTE_C_SHARP	4
#define NOTE_D			5
#define NOTE_D_SHARP	6
#define NOTE_E			7
#define NOTE_F			8
#define NOTE_F_SHARP	9
#define NOTE_G			10
#define NOTE_G_SHARP	11

#define NOTE_A_FLAT		NOTE_G_SHARP
#define NOTE_B_FLAT		NOTE_A_SHARP
#define NOTE_D_FLAT		NOTE_C_SHARP
#define NOTE_E_FLAT		NOTE_D_SHARP
#define NOTE_G_FLAT		NOTE_F_SHARP

#define BEAT_WHOLE		1
#define BEAT_HALF		2
#define BEAT_QUARTER	4
#define BEAT_EIGHTH		8
#define BEAT_SIXTEENTH	16

typedef struct snd_note_s {
	/* the note within the octave */
	unsigned int		note;
	/* the octave of the note */
	unsigned int		octave;

	/* time this note should play at (track time) */
	unsigned int		start_milliseconds;
	/* amount of time this note will be sounded for */
	unsigned int		duration_milliseconds;
} snd_note_t;
typedef struct snd_track_s {
	size_t				num_notes;
	snd_note_t *		notes;
	double				volume;
	vmcontext_t *		shader;
} snd_track_t;
typedef struct snd_active_note_s {
	snd_track_t *		track;
	snd_note_t *		note;
	unsigned int		start_sample;
	unsigned int		end_sample;
} snd_active_note_t;
typedef struct snd_context_s {
	size_t				num_tracks;
	snd_track_t *		tracks;

	size_t				num_shaders;
	vmcontext_t			shaders[ 32 ];

	double				volume;

	unsigned int		total_samples;
	unsigned int		samples_per_second;

	unsigned int		current_ms;
	unsigned int		current_sample;
	short *				samples;

	snd_note_t *		track_offsets[ 32 ];
	size_t				num_active_notes;
	snd_active_note_t	active_notes[ 32 ];
} snd_context_t;

/* calculate the frequency for a given note */
double snd_notefreq( unsigned int note, unsigned int octave )
{
	return pow( 2.0, ( double )( note + ( octave - 1 )*12 - 24 )/12.0 )*110.0;
}
/* convert a note duration (e.g., 1/8th) into a duration in milliseconds */
unsigned int snd_notetime( unsigned int duration, double tempo )
{
	return ( unsigned int )( 1.0/( double )duration*60.0/tempo*1000 );
}

/* sample a sound shader */
double snd_samplef( vmcontext_t *vm, double w, double t )
{
	assert( vm != NULL );
	assert( vm->numregs >= 2 );

	vm->regs[ 0 ] = w;
	vm->regs[ 1 ] = fmod( t, 1.0 );
	vm->regs[ 2 ] = t;
	vm->regs[ 3 ] = _M_PI;
	vm->regs[ 4 ] = fmod( t, 1.0 )*( 2.0*_M_PI );

	return vmeval( vm );
}

/* get an appropriate WAVE sample */
short snd_sample16( vmcontext_t *vm, double w, double t, double volume )
{
	return ( short )( ( volume - 0.5 )*32767*snd_samplef( vm, w, t ) );
}

/* step a sound context (returns 0 to 100 -- percentage complete) */
int snd_step( snd_context_t *snd )
{
	size_t i, num_samples;
	unsigned int current_ms;
	int value;
	short samples[ 32 ];
	int mschanged;
	int progress;

	if( snd->current_sample == snd->total_samples ) {
		return 100;
	}

	current_ms = snd->current_sample*1000/snd->samples_per_second;
	mschanged = current_ms != snd->current_ms;
	snd->current_ms = current_ms;

	/* find notes to play */
	if( mschanged ) {
		for( i = 0; i < snd->num_tracks; ++i ) {
			/* ignore end-of-track */
			if( !snd->track_offsets[ i ] ) {
				continue;
			}

			/* try adding notes (while loop allows chords to all play at once) */
			while( snd->track_offsets[ i ]->start_milliseconds == current_ms ) {
				/* add the note to the active set */
				if( snd->num_active_notes < countof( snd->active_notes ) ) {
					snd_active_note_t *an;

					an = &snd->active_notes[ snd->num_active_notes++ ];
					
					an->track = &snd->tracks[ i ];
					an->note = snd->track_offsets[ i ];
					an->start_sample = snd->current_sample;
					an->end_sample = snd->samples_per_second*an->note->duration_milliseconds/1000 + an->start_sample;
				}

				/* skip to the next note */
				++snd->track_offsets[ i ];
				if( snd->track_offsets[ i ] == snd->tracks[ i ].notes + snd->tracks[ i ].num_notes ) {
					snd->track_offsets[ i ] = NULL;
					break;
				}
			}
		}
	}

	/* sample the active notes, then turn off expired notes */
	num_samples = snd->num_active_notes;
	for( i = 0; i < snd->num_active_notes; ++i ) {
		double w, t;

		w = snd_notefreq( snd->active_notes[ i ].note->note, snd->active_notes[ i ].note->octave );
		t = ( double )( snd->current_sample - snd->active_notes[ i ].start_sample )/( double )snd->samples_per_second;

		samples[ i ] = snd_sample16( snd->active_notes[ i ].track->shader, w, t, snd->active_notes[ i ].track->volume );

		/* turn off the note if its time is out */
		if( snd->current_sample > snd->active_notes[ i ].end_sample ) {
			if( i != snd->num_active_notes - 1 ) {
				snd->active_notes[ i ] = snd->active_notes[ snd->num_active_notes - 1 ];
			}
			--snd->num_active_notes;
		}
	}

	/* mix the samples */
	value = 0;
	for( i = 0; i < num_samples; ++i ) {
		value += samples[ i ];
	}
#if 0
	value = ( int )( ( double )value/( double )num_samples*snd->volume );
#else
	value = ( int )( ( double )value*snd->volume );
#endif
	if( value < -32768 ) {
		value = -32768;
	} else if( value > 32767 ) {
		value = 32767;
	}

	snd->samples[ snd->current_sample ] = ( short )value;

	/* calculate progress and step the sample */
	progress = ( int )( snd->current_sample*100/snd->total_samples );
	++snd->current_sample;

	/* done */
	return progress < 100 ? progress : 99; /* 100 is reserved for "completely done" */
}

/* evaluate an entire sound */
void snd_eval( snd_context_t *snd, int showprogress )
{
	int prevprogress = -1;

	assert( snd != NULL );
	assert( snd->num_tracks > 0 );
	assert( snd->tracks != NULL );
	assert( snd->samples != NULL );

	for(;;) {
		int progress;

		assert( snd->current_sample <= snd->total_samples );

		progress = snd_step( snd );
		if( showprogress && progress != prevprogress ) {
			prevprogress = progress;
			
			printf( "Generating sound... %i%% (%u/%u)                  \r",
				progress, snd->current_sample, snd->total_samples );
		}

		if( progress==100 ) {
			break;
		}
	}

	if( showprogress ) {
		printf( "\n" );
	}
}

/* generate a wave file */
int snd_write( snd_context_t *snd, const char *filename, int showprogress )
{
	unsigned int data_size;
	unsigned int size;
	unsigned int fmt_size;
	unsigned short compression_code;
	unsigned short num_channels;
	unsigned int sample_rate;
	unsigned int avg_bytes_per_second;
	unsigned short block_align;
	unsigned short bits_per_sample;
	FILE *fp;

	assert( snd != NULL );
	assert( snd->samples != NULL );

	fp = fopen( filename, "wb" );
	if( !fp ) {
		fprintf( stderr, "[%s] ERROR: fopen() failed\n", filename );
		return 0;
	}

	snd_eval( snd, showprogress );

	data_size = snd->total_samples*sizeof( short );
	size = data_size - 8;
	fmt_size = 16;
	compression_code = 1; /*PCM*/
	num_channels = 1;
	sample_rate = snd->samples_per_second;
	avg_bytes_per_second = sample_rate*sizeof( short );
	block_align = 2;
	bits_per_sample = 16;

	fwrite( ( const void * )"RIFF", 4, 1, fp );
	fwrite( &size, 4, 1, fp );
	fwrite( ( const void * )"WAVE", 4, 1, fp );
	fwrite( ( const void * )"fmt ", 4, 1, fp );
	fwrite( &fmt_size, 4, 1, fp );
	fwrite( &compression_code, 2, 1, fp );
	fwrite( &num_channels, 2, 1, fp );
	fwrite( &sample_rate, 4, 1, fp );
	fwrite( &avg_bytes_per_second, 4, 1, fp );
	fwrite( &block_align, 2, 1, fp );
	fwrite( &bits_per_sample, 2, 1, fp );

	fwrite( ( const void * )"data", 4, 1, fp );
	fwrite( &data_size, 4, 1, fp );

	fwrite( ( const void * )snd->samples, ( size_t )data_size, 1, fp );

	fclose( fp );
	return 1;
}

/* generate a sine wave to a file from a sound script */
int snd_write_sine( const char *wavfilename, const char *sndfilename )
{
	snd_context_t *snd;
	program_t *prog;
	source_t *src;

	assert( wavfilename != NULL );
	assert( sndfilename != NULL );

	src = src_open( sndfilename );
	if( !src ) {
		return 0;
	}

	prog = prog_compile( src, NULL /* no ASM listing */ );
	if( !prog ) {
		mem_free( ( void * )src );
		return 0;
	}

	snd = ( snd_context_t * )mem_alloc( sizeof( *snd ) );
	if( !snd ) {
		fprintf( stderr, "[%s] ERROR: Out of memory\n", wavfilename );
		return 0;
	}

	mem_attach( ( void * )snd, ( void * )src );

	snd->num_tracks = 1;
	snd->tracks = ( snd_track_t * )mem_alloc( sizeof( snd_track_t )*snd->num_tracks );
	if( !snd->tracks ) {
		fprintf( stderr, "[%s] ERROR: Out of memory\n", wavfilename );
		mem_free( ( void * )src );
		return 0;
	}

	mem_attach( ( void * )snd->tracks, ( void * )snd );

	snd->tracks[0].num_notes = 8;
	snd->tracks[0].notes = ( snd_note_t * )mem_alloc( sizeof( snd_note_t )*snd->tracks[0].num_notes );
	if( !snd->tracks[0].notes ) {
		fprintf( stderr, "[%s] ERROR: Out of memory\n", wavfilename );
		mem_free( ( void * )src );
		return 0;
	}
	mem_attach( ( void * )snd->tracks[0].notes, ( void * )snd->tracks );

	snd->tracks[0].volume = 1.0;
	snd->tracks[0].shader = &snd->shaders[0];

	snd->tracks[0].notes[0].note = NOTE_G;
	snd->tracks[0].notes[0].octave = 4;
	snd->tracks[0].notes[0].start_milliseconds = 0;
	snd->tracks[0].notes[0].duration_milliseconds = 990;

	snd->tracks[0].notes[1].note = NOTE_C;
	snd->tracks[0].notes[1].octave = 4;
	snd->tracks[0].notes[1].start_milliseconds = 250;
	snd->tracks[0].notes[1].duration_milliseconds = 990 - 250;

	snd->tracks[0].notes[2].note = NOTE_C;
	snd->tracks[0].notes[2].octave = 5;
	snd->tracks[0].notes[2].start_milliseconds = 500;
	snd->tracks[0].notes[2].duration_milliseconds = 2000;

	snd->tracks[0].notes[3].note = NOTE_G;
	snd->tracks[0].notes[3].octave = 5;
	snd->tracks[0].notes[3].start_milliseconds = snd->tracks[0].notes[2].start_milliseconds;
	snd->tracks[0].notes[3].duration_milliseconds = snd->tracks[0].notes[2].duration_milliseconds;

	snd->tracks[0].notes[4].note = NOTE_E;
	snd->tracks[0].notes[4].octave = 3;
	snd->tracks[0].notes[4].start_milliseconds = 800;
	snd->tracks[0].notes[4].duration_milliseconds = 190;

	snd->tracks[0].notes[5].note = NOTE_D;
	snd->tracks[0].notes[5].octave = 3;
	snd->tracks[0].notes[5].start_milliseconds = 1000;
	snd->tracks[0].notes[5].duration_milliseconds = 2000;

	snd->tracks[0].notes[6].note = NOTE_A;
	snd->tracks[0].notes[6].octave = 4;
	snd->tracks[0].notes[6].start_milliseconds = 2000;
	snd->tracks[0].notes[6].duration_milliseconds = 250;

	snd->tracks[0].notes[7].note = snd->tracks[0].notes[6].note;
	snd->tracks[0].notes[7].octave = snd->tracks[0].notes[6].octave;
	snd->tracks[0].notes[7].start_milliseconds = snd->tracks[0].notes[6].start_milliseconds + snd->tracks[0].notes[6].duration_milliseconds;
	snd->tracks[0].notes[7].duration_milliseconds = snd->tracks[0].notes[6].duration_milliseconds;

	snd->num_shaders = 1;
	if( !vminit( &snd->shaders[0], prog->vm_indexes ) ) {
		src_error( src, "vminit() failed" );
		mem_free( ( void * )src );
		return 0;
	}

	install_vm_funcs( &snd->shaders[0] );

	snd->shaders[0].instr_s = prog->instrs;
	snd->shaders[0].instr_e = prog->instrs + prog->num_instrs;

	snd->volume = 0.8;

	snd->samples_per_second = 44100;
	snd->total_samples = snd->samples_per_second*3;

	snd->current_ms = ~( unsigned int )0;
	snd->current_sample = 0;

	snd->samples = ( short * )mem_alloc( snd->total_samples*sizeof( short ) );
	if( !snd->samples ) {
		fprintf( stderr, "[%s] ERROR: Out of memory\n", wavfilename );
		return 0;
	}

	mem_attach( ( void * )snd->samples, ( void * )snd );

	snd->track_offsets[0] = snd->tracks[0].notes;

	snd_write( snd, wavfilename, 1/*show-progress*/ );

	vmfini( &snd->shaders[0] );
	mem_free( ( void * )src );
	return 1;
}


/*
===============================================================================

	FRONT-END

===============================================================================
*/

int main( int argc, char **argv )
{
	int i;
	int exitcode = EXIT_SUCCESS;
	const char *outfile = NULL;

	for( i = 1; i < argc; ++i ) {
		if( strcmp( argv[ i ], "-o" ) == 0 ) {
			outfile = argv[ ++i ];
			continue;
		}

		if( outfile != NULL ) {
			if( !snd_write_sine( outfile, argv[ i ] ) ) {
				fprintf( stderr, "ERROR: snd_write_sine() failed\n" );
				exitcode = EXIT_FAILURE;
			}
		} else {
			if( !compilefile( argv[ i ] ) ) {
				fprintf( stderr, "ERROR: compilefile() failed\n" );
				exitcode = EXIT_FAILURE;
			}
		}
	}

	return exitcode;
}

