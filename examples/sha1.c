# 1 "sha1-example.c"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "sha1-example.c"




# 1 "write-be32.c" 1
# 27 "write-be32.c"
# 1 "/usr/include/stdlib.h" 1 3 4
# 24 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 378 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 385 "/usr/include/sys/cdefs.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 386 "/usr/include/sys/cdefs.h" 2 3 4
# 379 "/usr/include/features.h" 2 3 4
# 402 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4
# 10 "/usr/include/gnu/stubs.h" 3 4
# 1 "/usr/include/gnu/stubs-64.h" 1 3 4
# 11 "/usr/include/gnu/stubs.h" 2 3 4
# 403 "/usr/include/features.h" 2 3 4
# 25 "/usr/include/stdlib.h" 2 3 4







# 1 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stddef.h" 1 3 4
# 212 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 324 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stddef.h" 3 4
typedef int wchar_t;
# 33 "/usr/include/stdlib.h" 2 3 4








# 1 "/usr/include/bits/waitflags.h" 1 3 4
# 42 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/bits/waitstatus.h" 1 3 4
# 64 "/usr/include/bits/waitstatus.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 36 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 2 3 4
# 60 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/byteswap.h" 1 3 4
# 27 "/usr/include/bits/byteswap.h" 3 4
# 1 "/usr/include/bits/types.h" 1 3 4
# 27 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 28 "/usr/include/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;







typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
# 121 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 122 "/usr/include/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;
# 28 "/usr/include/bits/byteswap.h" 2 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 29 "/usr/include/bits/byteswap.h" 2 3 4






# 1 "/usr/include/bits/byteswap-16.h" 1 3 4
# 36 "/usr/include/bits/byteswap.h" 2 3 4
# 44 "/usr/include/bits/byteswap.h" 3 4
static __inline unsigned int
__bswap_32 (unsigned int __bsx)
{
  return __builtin_bswap32 (__bsx);
}
# 108 "/usr/include/bits/byteswap.h" 3 4
static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{
  return __builtin_bswap64 (__bsx);
}
# 61 "/usr/include/endian.h" 2 3 4
# 65 "/usr/include/bits/waitstatus.h" 2 3 4

union wait
  {
    int w_status;
    struct
      {

 unsigned int __w_termsig:7;
 unsigned int __w_coredump:1;
 unsigned int __w_retcode:8;
 unsigned int:16;







      } __wait_terminated;
    struct
      {

 unsigned int __w_stopval:8;
 unsigned int __w_stopsig:8;
 unsigned int:16;






      } __wait_stopped;
  };
# 43 "/usr/include/stdlib.h" 2 3 4
# 67 "/usr/include/stdlib.h" 3 4
typedef union
  {
    union wait *__uptr;
    int *__iptr;
  } __WAIT_STATUS __attribute__ ((__transparent_union__));
# 95 "/usr/include/stdlib.h" 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;







__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;


# 139 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__ , __leaf__)) ;




extern double atof (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern int atoi (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern long int atol (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;





__extension__ extern long long int atoll (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;





extern double strtod (const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern float strtof (const char *__restrict __nptr,
       char **__restrict __endptr) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern long double strtold (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern unsigned long int strtoul (const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int strtoq (const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





__extension__
extern long long int strtoll (const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

# 305 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) __attribute__ ((__nothrow__ , __leaf__)) ;


extern long int a64l (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;




# 1 "/usr/include/sys/types.h" 1 3 4
# 27 "/usr/include/sys/types.h" 3 4






typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;




typedef __loff_t loff_t;



typedef __ino_t ino_t;
# 60 "/usr/include/sys/types.h" 3 4
typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;





typedef __off_t off_t;
# 98 "/usr/include/sys/types.h" 3 4
typedef __pid_t pid_t;





typedef __id_t id_t;




typedef __ssize_t ssize_t;





typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;
# 132 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/time.h" 1 3 4
# 57 "/usr/include/time.h" 3 4


typedef __clock_t clock_t;



# 73 "/usr/include/time.h" 3 4


typedef __time_t time_t;



# 91 "/usr/include/time.h" 3 4
typedef __clockid_t clockid_t;
# 103 "/usr/include/time.h" 3 4
typedef __timer_t timer_t;
# 133 "/usr/include/sys/types.h" 2 3 4
# 146 "/usr/include/sys/types.h" 3 4
# 1 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stddef.h" 1 3 4
# 147 "/usr/include/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
# 194 "/usr/include/sys/types.h" 3 4
typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));


typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));
# 219 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/sys/select.h" 1 3 4
# 30 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 22 "/usr/include/bits/select.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 23 "/usr/include/bits/select.h" 2 3 4
# 31 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/sigset.h" 1 3 4
# 22 "/usr/include/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 34 "/usr/include/sys/select.h" 2 3 4



typedef __sigset_t sigset_t;





# 1 "/usr/include/time.h" 1 3 4
# 120 "/usr/include/time.h" 3 4
struct timespec
  {
    __time_t tv_sec;
    __syscall_slong_t tv_nsec;
  };
# 44 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/bits/time.h" 1 3 4
# 30 "/usr/include/bits/time.h" 3 4
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
# 46 "/usr/include/sys/select.h" 2 3 4


typedef __suseconds_t suseconds_t;





typedef long int __fd_mask;
# 64 "/usr/include/sys/select.h" 3 4
typedef struct
  {






    __fd_mask __fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];


  } fd_set;






typedef __fd_mask fd_mask;
# 96 "/usr/include/sys/select.h" 3 4

# 106 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 118 "/usr/include/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
# 131 "/usr/include/sys/select.h" 3 4

# 220 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/sysmacros.h" 1 3 4
# 24 "/usr/include/sys/sysmacros.h" 3 4


__extension__
extern unsigned int gnu_dev_major (unsigned long long int __dev)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
__extension__
extern unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
__extension__
extern unsigned long long int gnu_dev_makedev (unsigned int __major,
            unsigned int __minor)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
# 58 "/usr/include/sys/sysmacros.h" 3 4

# 223 "/usr/include/sys/types.h" 2 3 4





typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 270 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/bits/pthreadtypes.h" 1 3 4
# 21 "/usr/include/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 22 "/usr/include/bits/pthreadtypes.h" 2 3 4
# 60 "/usr/include/bits/pthreadtypes.h" 3 4
typedef unsigned long int pthread_t;


union pthread_attr_t
{
  char __size[56];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;





typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 90 "/usr/include/bits/pthreadtypes.h" 3 4
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;

    unsigned int __nusers;



    int __kind;

    short __spins;
    short __elision;
    __pthread_list_t __list;
# 124 "/usr/include/bits/pthreadtypes.h" 3 4
  } __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  struct
  {
    int __lock;
    unsigned int __futex;
    __extension__ unsigned long long int __total_seq;
    __extension__ unsigned long long int __wakeup_seq;
    __extension__ unsigned long long int __woken_seq;
    void *__mutex;
    unsigned int __nwaiters;
    unsigned int __broadcast_seq;
  } __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;





typedef union
{

  struct
  {
    int __lock;
    unsigned int __nr_readers;
    unsigned int __readers_wakeup;
    unsigned int __writer_wakeup;
    unsigned int __nr_readers_queued;
    unsigned int __nr_writers_queued;
    int __writer;
    int __shared;
    unsigned long int __pad1;
    unsigned long int __pad2;


    unsigned int __flags;

  } __data;
# 211 "/usr/include/bits/pthreadtypes.h" 3 4
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 271 "/usr/include/sys/types.h" 2 3 4



# 315 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) __attribute__ ((__nothrow__ , __leaf__));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__ , __leaf__));





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));






extern int rand (void) __attribute__ ((__nothrow__ , __leaf__));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__ , __leaf__));




extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__ , __leaf__));







extern double drand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__ , __leaf__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    __extension__ unsigned long long int __a;

  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));









extern void *malloc (size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;










extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__warn_unused_result__));

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));




extern void cfree (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));



# 1 "/usr/include/alloca.h" 1 3 4
# 24 "/usr/include/alloca.h" 3 4
# 1 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stddef.h" 1 3 4
# 25 "/usr/include/alloca.h" 2 3 4







extern void *alloca (size_t __size) __attribute__ ((__nothrow__ , __leaf__));






# 493 "/usr/include/stdlib.h" 2 3 4





extern void *valloc (size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 513 "/usr/include/stdlib.h" 3 4


extern void abort (void) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 530 "/usr/include/stdlib.h" 3 4





extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));






extern void exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));













extern void _Exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));






extern char *getenv (const char *__name) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;

# 578 "/usr/include/stdlib.h" 3 4
extern int putenv (char *__string) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern int setenv (const char *__name, const char *__value, int __replace)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));


extern int unsetenv (const char *__name) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));






extern int clearenv (void) __attribute__ ((__nothrow__ , __leaf__));
# 606 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 620 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 642 "/usr/include/stdlib.h" 3 4
extern int mkstemps (char *__template, int __suffixlen) __attribute__ ((__nonnull__ (1))) ;
# 663 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 712 "/usr/include/stdlib.h" 3 4





extern int system (const char *__command) ;

# 734 "/usr/include/stdlib.h" 3 4
extern char *realpath (const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__ , __leaf__)) ;






typedef int (*__compar_fn_t) (const void *, const void *);
# 752 "/usr/include/stdlib.h" 3 4



extern void *bsearch (const void *__key, const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) ;







extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));
# 775 "/usr/include/stdlib.h" 3 4
extern int abs (int __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
extern long int labs (long int __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;



__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;







extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;




__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;

# 812 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;




extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3))) ;




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3))) ;




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));






extern int mblen (const char *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));


extern int mbtowc (wchar_t *__restrict __pwc,
     const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__ , __leaf__));



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));

extern size_t wcstombs (char *__restrict __s,
   const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__));








extern int rpmatch (const char *__response) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 899 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
        char *const *__restrict __tokens,
        char **__restrict __valuep)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2, 3))) ;
# 951 "/usr/include/stdlib.h" 3 4
extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


# 1 "/usr/include/bits/stdlib-float.h" 1 3 4
# 956 "/usr/include/stdlib.h" 2 3 4
# 968 "/usr/include/stdlib.h" 3 4

# 28 "write-be32.c" 2

# 1 "nettle-write.h" 1
# 29 "nettle-write.h"
# 1 "nettle-stdint.h" 1
# 11 "nettle-stdint.h"
# 1 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stdint.h" 1 3 4
# 9 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stdint.h" 3 4
# 1 "/usr/include/stdint.h" 1 3 4
# 26 "/usr/include/stdint.h" 3 4
# 1 "/usr/include/bits/wchar.h" 1 3 4
# 27 "/usr/include/stdint.h" 2 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 28 "/usr/include/stdint.h" 2 3 4
# 48 "/usr/include/stdint.h" 3 4
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;

typedef unsigned int uint32_t;



typedef unsigned long int uint64_t;
# 65 "/usr/include/stdint.h" 3 4
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;

typedef long int int_least64_t;






typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;

typedef unsigned long int uint_least64_t;
# 90 "/usr/include/stdint.h" 3 4
typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 103 "/usr/include/stdint.h" 3 4
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 119 "/usr/include/stdint.h" 3 4
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 134 "/usr/include/stdint.h" 3 4
typedef long int intmax_t;
typedef unsigned long int uintmax_t;
# 10 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stdint.h" 2 3 4
# 12 "nettle-stdint.h" 2
# 30 "nettle-write.h" 2






void
_nettle_write_be32(unsigned length, uint8_t *dst,
     uint32_t *src);
void
_nettle_write_le32(unsigned length, uint8_t *dst,
     uint32_t *src);

void
_nettle_write_le64(unsigned length, uint8_t *dst,
     uint64_t *src);
# 30 "write-be32.c" 2

# 1 "macros.h" 1
# 32 "write-be32.c" 2

void
_nettle_write_be32(unsigned length, uint8_t *dst,
     uint32_t *src)
{
  unsigned i;
  unsigned words;
  unsigned leftover;

  words = length / 4;
  leftover = length % 4;

  for (i = 0; i < words; i++, dst += 4)
    do { (dst)[0] = ((src[i]) >> 24) & 0xff; (dst)[1] = ((src[i]) >> 16) & 0xff; (dst)[2] = ((src[i]) >> 8) & 0xff; (dst)[3] = (src[i]) & 0xff; } while(0);

  if (leftover)
    {
      uint32_t word;
      unsigned j = leftover;

      word = src[i];

      switch (leftover)
 {
 default:
   abort();
 case 3:
   dst[--j] = (word >> 8) & 0xff;

 case 2:
   dst[--j] = (word >> 16) & 0xff;

 case 1:
   dst[--j] = (word >> 24) & 0xff;
 }
    }
}
# 6 "sha1-example.c" 2
# 1 "sha1-compress.c" 1
# 54 "sha1-compress.c"
# 1 "/usr/include/assert.h" 1 3 4
# 64 "/usr/include/assert.h" 3 4



extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



# 55 "sha1-compress.c" 2

# 1 "/usr/include/string.h" 1 3 4
# 27 "/usr/include/string.h" 3 4





# 1 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include/stddef.h" 1 3 4
# 33 "/usr/include/string.h" 2 3 4
# 44 "/usr/include/string.h" 3 4


extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void *memmove (void *__dest, const void *__src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));






extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
        int __c, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));





extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int memcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 96 "/usr/include/string.h" 3 4
extern void *memchr (const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));


# 127 "/usr/include/string.h" 3 4


extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern char *strcat (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncat (char *__restrict __dest, const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern int strncmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcoll (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern size_t strxfrm (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));






# 1 "/usr/include/xlocale.h" 1 3 4
# 27 "/usr/include/xlocale.h" 3 4
typedef struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
} *__locale_t;


typedef __locale_t locale_t;
# 164 "/usr/include/string.h" 2 3 4


extern int strcoll_l (const char *__s1, const char *__s2, __locale_t __l)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));

extern size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
    __locale_t __l) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)));





extern char *strdup (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));






extern char *strndup (const char *__string, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
# 211 "/usr/include/string.h" 3 4

# 236 "/usr/include/string.h" 3 4
extern char *strchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 263 "/usr/include/string.h" 3 4
extern char *strrchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));


# 282 "/usr/include/string.h" 3 4



extern size_t strcspn (const char *__s, const char *__reject)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern size_t strspn (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 315 "/usr/include/string.h" 3 4
extern char *strpbrk (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 342 "/usr/include/string.h" 3 4
extern char *strstr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strtok (char *__restrict __s, const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));




extern char *__strtok_r (char *__restrict __s,
    const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));

extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));
# 397 "/usr/include/string.h" 3 4


extern size_t strlen (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern size_t strnlen (const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern char *strerror (int __errnum) __attribute__ ((__nothrow__ , __leaf__));

# 427 "/usr/include/string.h" 3 4
extern int strerror_r (int __errnum, char *__buf, size_t __buflen) __asm__ ("" "__xpg_strerror_r") __attribute__ ((__nothrow__ , __leaf__))

                        __attribute__ ((__nonnull__ (2)));
# 445 "/usr/include/string.h" 3 4
extern char *strerror_l (int __errnum, __locale_t __l) __attribute__ ((__nothrow__ , __leaf__));





extern void __bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern void bcopy (const void *__src, void *__dest, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int bcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 489 "/usr/include/string.h" 3 4
extern char *index (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 517 "/usr/include/string.h" 3 4
extern char *rindex (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern int ffs (int __i) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
# 534 "/usr/include/string.h" 3 4
extern int strcasecmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strncasecmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 557 "/usr/include/string.h" 3 4
extern char *strsep (char **__restrict __stringp,
       const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strsignal (int __sig) __attribute__ ((__nothrow__ , __leaf__));


extern char *__stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));



extern char *__stpncpy (char *__restrict __dest,
   const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
# 644 "/usr/include/string.h" 3 4

# 57 "sha1-compress.c" 2

# 1 "sha1.h" 1
# 29 "sha1.h"
# 1 "nettle-types.h" 1
# 36 "nettle-types.h"
typedef void nettle_random_func(void *ctx,
    unsigned length, uint8_t *dst);


typedef void nettle_progress_func(void *ctx, int c);


typedef void *nettle_realloc_func(void *ctx, void *p, unsigned length);


typedef void nettle_set_key_func(void *ctx,
     unsigned length,
     const uint8_t *key);







typedef void nettle_crypt_func(void *ctx,
          unsigned length, uint8_t *dst,
          const uint8_t *src);


typedef void nettle_hash_init_func(void *ctx);
typedef void nettle_hash_update_func(void *ctx,
         unsigned length,
         const uint8_t *src);
typedef void nettle_hash_digest_func(void *ctx,
         unsigned length, uint8_t *dst);



typedef unsigned nettle_armor_length_func(unsigned length);
typedef void nettle_armor_init_func(void *ctx);

typedef unsigned nettle_armor_encode_update_func(void *ctx,
       uint8_t *dst,
       unsigned src_length,
       const uint8_t *src);

typedef unsigned nettle_armor_encode_final_func(void *ctx, uint8_t *dst);

typedef int nettle_armor_decode_update_func(void *ctx,
         unsigned *dst_length,
         uint8_t *dst,
         unsigned src_length,
         const uint8_t *src);

typedef int nettle_armor_decode_final_func(void *ctx);
# 30 "sha1.h" 2
# 48 "sha1.h"
struct sha1_ctx
{
  uint32_t state[5];
  uint32_t count_low, count_high;
  uint8_t block[64];
  unsigned int index;
};

void
nettle_sha1_init(struct sha1_ctx *ctx);

void
nettle_sha1_update(struct sha1_ctx *ctx,
     unsigned length,
     const uint8_t *data);

void
nettle_sha1_digest(struct sha1_ctx *ctx,
     unsigned length,
     uint8_t *digest);



void
_nettle_sha1_compress(uint32_t *state, const uint8_t *data);
# 59 "sha1-compress.c" 2
# 129 "sha1-compress.c"
void
_nettle_sha1_compress(uint32_t *state, const uint8_t *input)
{
  uint32_t data[16];
  uint32_t A, B, C, D, E;
  int i;

  for (i = 0; i < 16; i++, input+= 4)
    {
      data[i] = ( (((uint32_t) (input)[0]) << 24) | (((uint32_t) (input)[1]) << 16) | (((uint32_t) (input)[2]) << 8) | ((uint32_t) (input)[3]));
    }


  A = state[0];
  B = state[1];
  C = state[2];
  D = state[3];
  E = state[4];

  ;

  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( D ^ ( B & ( C ^ D ) ) ) + 0x5A827999L + data[ 0], B = (((B)<<(30)) | ((B)>>(32-(30)))) ); ;
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( C ^ ( A & ( B ^ C ) ) ) + 0x5A827999L + data[ 1], A = (((A)<<(30)) | ((A)>>(32-(30)))) ); ;
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( B ^ ( E & ( A ^ B ) ) ) + 0x5A827999L + data[ 2], E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( A ^ ( D & ( E ^ A ) ) ) + 0x5A827999L + data[ 3], D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( E ^ ( C & ( D ^ E ) ) ) + 0x5A827999L + data[ 4], C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( D ^ ( B & ( C ^ D ) ) ) + 0x5A827999L + data[ 5], B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( C ^ ( A & ( B ^ C ) ) ) + 0x5A827999L + data[ 6], A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( B ^ ( E & ( A ^ B ) ) ) + 0x5A827999L + data[ 7], E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( A ^ ( D & ( E ^ A ) ) ) + 0x5A827999L + data[ 8], D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( E ^ ( C & ( D ^ E ) ) ) + 0x5A827999L + data[ 9], C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( D ^ ( B & ( C ^ D ) ) ) + 0x5A827999L + data[10], B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( C ^ ( A & ( B ^ C ) ) ) + 0x5A827999L + data[11], A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( B ^ ( E & ( A ^ B ) ) ) + 0x5A827999L + data[12], E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( A ^ ( D & ( E ^ A ) ) ) + 0x5A827999L + data[13], D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( E ^ ( C & ( D ^ E ) ) ) + 0x5A827999L + data[14], C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( D ^ ( B & ( C ^ D ) ) ) + 0x5A827999L + data[15], B = (((B)<<(30)) | ((B)>>(32-(30)))) ); ;
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( C ^ ( A & ( B ^ C ) ) ) + 0x5A827999L + ( data[ 16 & 15 ] = (((( data[ 16 & 15 ] ^ data[ (16 - 14) & 15 ] ^ data[ (16 - 8) & 15 ] ^ data[ (16 - 3) & 15 ] ))<<(1)) | ((( data[ 16 & 15 ] ^ data[ (16 - 14) & 15 ] ^ data[ (16 - 8) & 15 ] ^ data[ (16 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) ); ;
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( B ^ ( E & ( A ^ B ) ) ) + 0x5A827999L + ( data[ 17 & 15 ] = (((( data[ 17 & 15 ] ^ data[ (17 - 14) & 15 ] ^ data[ (17 - 8) & 15 ] ^ data[ (17 - 3) & 15 ] ))<<(1)) | ((( data[ 17 & 15 ] ^ data[ (17 - 14) & 15 ] ^ data[ (17 - 8) & 15 ] ^ data[ (17 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) ); ;
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( A ^ ( D & ( E ^ A ) ) ) + 0x5A827999L + ( data[ 18 & 15 ] = (((( data[ 18 & 15 ] ^ data[ (18 - 14) & 15 ] ^ data[ (18 - 8) & 15 ] ^ data[ (18 - 3) & 15 ] ))<<(1)) | ((( data[ 18 & 15 ] ^ data[ (18 - 14) & 15 ] ^ data[ (18 - 8) & 15 ] ^ data[ (18 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) ); ;
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( E ^ ( C & ( D ^ E ) ) ) + 0x5A827999L + ( data[ 19 & 15 ] = (((( data[ 19 & 15 ] ^ data[ (19 - 14) & 15 ] ^ data[ (19 - 8) & 15 ] ^ data[ (19 - 3) & 15 ] ))<<(1)) | ((( data[ 19 & 15 ] ^ data[ (19 - 14) & 15 ] ^ data[ (19 - 8) & 15 ] ^ data[ (19 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) ); ;

  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0x6ED9EBA1L + ( data[ 20 & 15 ] = (((( data[ 20 & 15 ] ^ data[ (20 - 14) & 15 ] ^ data[ (20 - 8) & 15 ] ^ data[ (20 - 3) & 15 ] ))<<(1)) | ((( data[ 20 & 15 ] ^ data[ (20 - 14) & 15 ] ^ data[ (20 - 8) & 15 ] ^ data[ (20 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) ); ;
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0x6ED9EBA1L + ( data[ 21 & 15 ] = (((( data[ 21 & 15 ] ^ data[ (21 - 14) & 15 ] ^ data[ (21 - 8) & 15 ] ^ data[ (21 - 3) & 15 ] ))<<(1)) | ((( data[ 21 & 15 ] ^ data[ (21 - 14) & 15 ] ^ data[ (21 - 8) & 15 ] ^ data[ (21 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) ); ;
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0x6ED9EBA1L + ( data[ 22 & 15 ] = (((( data[ 22 & 15 ] ^ data[ (22 - 14) & 15 ] ^ data[ (22 - 8) & 15 ] ^ data[ (22 - 3) & 15 ] ))<<(1)) | ((( data[ 22 & 15 ] ^ data[ (22 - 14) & 15 ] ^ data[ (22 - 8) & 15 ] ^ data[ (22 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0x6ED9EBA1L + ( data[ 23 & 15 ] = (((( data[ 23 & 15 ] ^ data[ (23 - 14) & 15 ] ^ data[ (23 - 8) & 15 ] ^ data[ (23 - 3) & 15 ] ))<<(1)) | ((( data[ 23 & 15 ] ^ data[ (23 - 14) & 15 ] ^ data[ (23 - 8) & 15 ] ^ data[ (23 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0x6ED9EBA1L + ( data[ 24 & 15 ] = (((( data[ 24 & 15 ] ^ data[ (24 - 14) & 15 ] ^ data[ (24 - 8) & 15 ] ^ data[ (24 - 3) & 15 ] ))<<(1)) | ((( data[ 24 & 15 ] ^ data[ (24 - 14) & 15 ] ^ data[ (24 - 8) & 15 ] ^ data[ (24 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0x6ED9EBA1L + ( data[ 25 & 15 ] = (((( data[ 25 & 15 ] ^ data[ (25 - 14) & 15 ] ^ data[ (25 - 8) & 15 ] ^ data[ (25 - 3) & 15 ] ))<<(1)) | ((( data[ 25 & 15 ] ^ data[ (25 - 14) & 15 ] ^ data[ (25 - 8) & 15 ] ^ data[ (25 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0x6ED9EBA1L + ( data[ 26 & 15 ] = (((( data[ 26 & 15 ] ^ data[ (26 - 14) & 15 ] ^ data[ (26 - 8) & 15 ] ^ data[ (26 - 3) & 15 ] ))<<(1)) | ((( data[ 26 & 15 ] ^ data[ (26 - 14) & 15 ] ^ data[ (26 - 8) & 15 ] ^ data[ (26 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0x6ED9EBA1L + ( data[ 27 & 15 ] = (((( data[ 27 & 15 ] ^ data[ (27 - 14) & 15 ] ^ data[ (27 - 8) & 15 ] ^ data[ (27 - 3) & 15 ] ))<<(1)) | ((( data[ 27 & 15 ] ^ data[ (27 - 14) & 15 ] ^ data[ (27 - 8) & 15 ] ^ data[ (27 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0x6ED9EBA1L + ( data[ 28 & 15 ] = (((( data[ 28 & 15 ] ^ data[ (28 - 14) & 15 ] ^ data[ (28 - 8) & 15 ] ^ data[ (28 - 3) & 15 ] ))<<(1)) | ((( data[ 28 & 15 ] ^ data[ (28 - 14) & 15 ] ^ data[ (28 - 8) & 15 ] ^ data[ (28 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0x6ED9EBA1L + ( data[ 29 & 15 ] = (((( data[ 29 & 15 ] ^ data[ (29 - 14) & 15 ] ^ data[ (29 - 8) & 15 ] ^ data[ (29 - 3) & 15 ] ))<<(1)) | ((( data[ 29 & 15 ] ^ data[ (29 - 14) & 15 ] ^ data[ (29 - 8) & 15 ] ^ data[ (29 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0x6ED9EBA1L + ( data[ 30 & 15 ] = (((( data[ 30 & 15 ] ^ data[ (30 - 14) & 15 ] ^ data[ (30 - 8) & 15 ] ^ data[ (30 - 3) & 15 ] ))<<(1)) | ((( data[ 30 & 15 ] ^ data[ (30 - 14) & 15 ] ^ data[ (30 - 8) & 15 ] ^ data[ (30 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0x6ED9EBA1L + ( data[ 31 & 15 ] = (((( data[ 31 & 15 ] ^ data[ (31 - 14) & 15 ] ^ data[ (31 - 8) & 15 ] ^ data[ (31 - 3) & 15 ] ))<<(1)) | ((( data[ 31 & 15 ] ^ data[ (31 - 14) & 15 ] ^ data[ (31 - 8) & 15 ] ^ data[ (31 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0x6ED9EBA1L + ( data[ 32 & 15 ] = (((( data[ 32 & 15 ] ^ data[ (32 - 14) & 15 ] ^ data[ (32 - 8) & 15 ] ^ data[ (32 - 3) & 15 ] ))<<(1)) | ((( data[ 32 & 15 ] ^ data[ (32 - 14) & 15 ] ^ data[ (32 - 8) & 15 ] ^ data[ (32 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0x6ED9EBA1L + ( data[ 33 & 15 ] = (((( data[ 33 & 15 ] ^ data[ (33 - 14) & 15 ] ^ data[ (33 - 8) & 15 ] ^ data[ (33 - 3) & 15 ] ))<<(1)) | ((( data[ 33 & 15 ] ^ data[ (33 - 14) & 15 ] ^ data[ (33 - 8) & 15 ] ^ data[ (33 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0x6ED9EBA1L + ( data[ 34 & 15 ] = (((( data[ 34 & 15 ] ^ data[ (34 - 14) & 15 ] ^ data[ (34 - 8) & 15 ] ^ data[ (34 - 3) & 15 ] ))<<(1)) | ((( data[ 34 & 15 ] ^ data[ (34 - 14) & 15 ] ^ data[ (34 - 8) & 15 ] ^ data[ (34 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0x6ED9EBA1L + ( data[ 35 & 15 ] = (((( data[ 35 & 15 ] ^ data[ (35 - 14) & 15 ] ^ data[ (35 - 8) & 15 ] ^ data[ (35 - 3) & 15 ] ))<<(1)) | ((( data[ 35 & 15 ] ^ data[ (35 - 14) & 15 ] ^ data[ (35 - 8) & 15 ] ^ data[ (35 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0x6ED9EBA1L + ( data[ 36 & 15 ] = (((( data[ 36 & 15 ] ^ data[ (36 - 14) & 15 ] ^ data[ (36 - 8) & 15 ] ^ data[ (36 - 3) & 15 ] ))<<(1)) | ((( data[ 36 & 15 ] ^ data[ (36 - 14) & 15 ] ^ data[ (36 - 8) & 15 ] ^ data[ (36 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0x6ED9EBA1L + ( data[ 37 & 15 ] = (((( data[ 37 & 15 ] ^ data[ (37 - 14) & 15 ] ^ data[ (37 - 8) & 15 ] ^ data[ (37 - 3) & 15 ] ))<<(1)) | ((( data[ 37 & 15 ] ^ data[ (37 - 14) & 15 ] ^ data[ (37 - 8) & 15 ] ^ data[ (37 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0x6ED9EBA1L + ( data[ 38 & 15 ] = (((( data[ 38 & 15 ] ^ data[ (38 - 14) & 15 ] ^ data[ (38 - 8) & 15 ] ^ data[ (38 - 3) & 15 ] ))<<(1)) | ((( data[ 38 & 15 ] ^ data[ (38 - 14) & 15 ] ^ data[ (38 - 8) & 15 ] ^ data[ (38 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) ); ;
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0x6ED9EBA1L + ( data[ 39 & 15 ] = (((( data[ 39 & 15 ] ^ data[ (39 - 14) & 15 ] ^ data[ (39 - 8) & 15 ] ^ data[ (39 - 3) & 15 ] ))<<(1)) | ((( data[ 39 & 15 ] ^ data[ (39 - 14) & 15 ] ^ data[ (39 - 8) & 15 ] ^ data[ (39 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) ); ;

  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( ( B & C ) | ( D & ( B | C ) ) ) + 0x8F1BBCDCL + ( data[ 40 & 15 ] = (((( data[ 40 & 15 ] ^ data[ (40 - 14) & 15 ] ^ data[ (40 - 8) & 15 ] ^ data[ (40 - 3) & 15 ] ))<<(1)) | ((( data[ 40 & 15 ] ^ data[ (40 - 14) & 15 ] ^ data[ (40 - 8) & 15 ] ^ data[ (40 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) ); ;
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( ( A & B ) | ( C & ( A | B ) ) ) + 0x8F1BBCDCL + ( data[ 41 & 15 ] = (((( data[ 41 & 15 ] ^ data[ (41 - 14) & 15 ] ^ data[ (41 - 8) & 15 ] ^ data[ (41 - 3) & 15 ] ))<<(1)) | ((( data[ 41 & 15 ] ^ data[ (41 - 14) & 15 ] ^ data[ (41 - 8) & 15 ] ^ data[ (41 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) ); ;
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( ( E & A ) | ( B & ( E | A ) ) ) + 0x8F1BBCDCL + ( data[ 42 & 15 ] = (((( data[ 42 & 15 ] ^ data[ (42 - 14) & 15 ] ^ data[ (42 - 8) & 15 ] ^ data[ (42 - 3) & 15 ] ))<<(1)) | ((( data[ 42 & 15 ] ^ data[ (42 - 14) & 15 ] ^ data[ (42 - 8) & 15 ] ^ data[ (42 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( ( D & E ) | ( A & ( D | E ) ) ) + 0x8F1BBCDCL + ( data[ 43 & 15 ] = (((( data[ 43 & 15 ] ^ data[ (43 - 14) & 15 ] ^ data[ (43 - 8) & 15 ] ^ data[ (43 - 3) & 15 ] ))<<(1)) | ((( data[ 43 & 15 ] ^ data[ (43 - 14) & 15 ] ^ data[ (43 - 8) & 15 ] ^ data[ (43 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( ( C & D ) | ( E & ( C | D ) ) ) + 0x8F1BBCDCL + ( data[ 44 & 15 ] = (((( data[ 44 & 15 ] ^ data[ (44 - 14) & 15 ] ^ data[ (44 - 8) & 15 ] ^ data[ (44 - 3) & 15 ] ))<<(1)) | ((( data[ 44 & 15 ] ^ data[ (44 - 14) & 15 ] ^ data[ (44 - 8) & 15 ] ^ data[ (44 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( ( B & C ) | ( D & ( B | C ) ) ) + 0x8F1BBCDCL + ( data[ 45 & 15 ] = (((( data[ 45 & 15 ] ^ data[ (45 - 14) & 15 ] ^ data[ (45 - 8) & 15 ] ^ data[ (45 - 3) & 15 ] ))<<(1)) | ((( data[ 45 & 15 ] ^ data[ (45 - 14) & 15 ] ^ data[ (45 - 8) & 15 ] ^ data[ (45 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( ( A & B ) | ( C & ( A | B ) ) ) + 0x8F1BBCDCL + ( data[ 46 & 15 ] = (((( data[ 46 & 15 ] ^ data[ (46 - 14) & 15 ] ^ data[ (46 - 8) & 15 ] ^ data[ (46 - 3) & 15 ] ))<<(1)) | ((( data[ 46 & 15 ] ^ data[ (46 - 14) & 15 ] ^ data[ (46 - 8) & 15 ] ^ data[ (46 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( ( E & A ) | ( B & ( E | A ) ) ) + 0x8F1BBCDCL + ( data[ 47 & 15 ] = (((( data[ 47 & 15 ] ^ data[ (47 - 14) & 15 ] ^ data[ (47 - 8) & 15 ] ^ data[ (47 - 3) & 15 ] ))<<(1)) | ((( data[ 47 & 15 ] ^ data[ (47 - 14) & 15 ] ^ data[ (47 - 8) & 15 ] ^ data[ (47 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( ( D & E ) | ( A & ( D | E ) ) ) + 0x8F1BBCDCL + ( data[ 48 & 15 ] = (((( data[ 48 & 15 ] ^ data[ (48 - 14) & 15 ] ^ data[ (48 - 8) & 15 ] ^ data[ (48 - 3) & 15 ] ))<<(1)) | ((( data[ 48 & 15 ] ^ data[ (48 - 14) & 15 ] ^ data[ (48 - 8) & 15 ] ^ data[ (48 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( ( C & D ) | ( E & ( C | D ) ) ) + 0x8F1BBCDCL + ( data[ 49 & 15 ] = (((( data[ 49 & 15 ] ^ data[ (49 - 14) & 15 ] ^ data[ (49 - 8) & 15 ] ^ data[ (49 - 3) & 15 ] ))<<(1)) | ((( data[ 49 & 15 ] ^ data[ (49 - 14) & 15 ] ^ data[ (49 - 8) & 15 ] ^ data[ (49 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( ( B & C ) | ( D & ( B | C ) ) ) + 0x8F1BBCDCL + ( data[ 50 & 15 ] = (((( data[ 50 & 15 ] ^ data[ (50 - 14) & 15 ] ^ data[ (50 - 8) & 15 ] ^ data[ (50 - 3) & 15 ] ))<<(1)) | ((( data[ 50 & 15 ] ^ data[ (50 - 14) & 15 ] ^ data[ (50 - 8) & 15 ] ^ data[ (50 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( ( A & B ) | ( C & ( A | B ) ) ) + 0x8F1BBCDCL + ( data[ 51 & 15 ] = (((( data[ 51 & 15 ] ^ data[ (51 - 14) & 15 ] ^ data[ (51 - 8) & 15 ] ^ data[ (51 - 3) & 15 ] ))<<(1)) | ((( data[ 51 & 15 ] ^ data[ (51 - 14) & 15 ] ^ data[ (51 - 8) & 15 ] ^ data[ (51 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( ( E & A ) | ( B & ( E | A ) ) ) + 0x8F1BBCDCL + ( data[ 52 & 15 ] = (((( data[ 52 & 15 ] ^ data[ (52 - 14) & 15 ] ^ data[ (52 - 8) & 15 ] ^ data[ (52 - 3) & 15 ] ))<<(1)) | ((( data[ 52 & 15 ] ^ data[ (52 - 14) & 15 ] ^ data[ (52 - 8) & 15 ] ^ data[ (52 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( ( D & E ) | ( A & ( D | E ) ) ) + 0x8F1BBCDCL + ( data[ 53 & 15 ] = (((( data[ 53 & 15 ] ^ data[ (53 - 14) & 15 ] ^ data[ (53 - 8) & 15 ] ^ data[ (53 - 3) & 15 ] ))<<(1)) | ((( data[ 53 & 15 ] ^ data[ (53 - 14) & 15 ] ^ data[ (53 - 8) & 15 ] ^ data[ (53 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( ( C & D ) | ( E & ( C | D ) ) ) + 0x8F1BBCDCL + ( data[ 54 & 15 ] = (((( data[ 54 & 15 ] ^ data[ (54 - 14) & 15 ] ^ data[ (54 - 8) & 15 ] ^ data[ (54 - 3) & 15 ] ))<<(1)) | ((( data[ 54 & 15 ] ^ data[ (54 - 14) & 15 ] ^ data[ (54 - 8) & 15 ] ^ data[ (54 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( ( B & C ) | ( D & ( B | C ) ) ) + 0x8F1BBCDCL + ( data[ 55 & 15 ] = (((( data[ 55 & 15 ] ^ data[ (55 - 14) & 15 ] ^ data[ (55 - 8) & 15 ] ^ data[ (55 - 3) & 15 ] ))<<(1)) | ((( data[ 55 & 15 ] ^ data[ (55 - 14) & 15 ] ^ data[ (55 - 8) & 15 ] ^ data[ (55 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( ( A & B ) | ( C & ( A | B ) ) ) + 0x8F1BBCDCL + ( data[ 56 & 15 ] = (((( data[ 56 & 15 ] ^ data[ (56 - 14) & 15 ] ^ data[ (56 - 8) & 15 ] ^ data[ (56 - 3) & 15 ] ))<<(1)) | ((( data[ 56 & 15 ] ^ data[ (56 - 14) & 15 ] ^ data[ (56 - 8) & 15 ] ^ data[ (56 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( ( E & A ) | ( B & ( E | A ) ) ) + 0x8F1BBCDCL + ( data[ 57 & 15 ] = (((( data[ 57 & 15 ] ^ data[ (57 - 14) & 15 ] ^ data[ (57 - 8) & 15 ] ^ data[ (57 - 3) & 15 ] ))<<(1)) | ((( data[ 57 & 15 ] ^ data[ (57 - 14) & 15 ] ^ data[ (57 - 8) & 15 ] ^ data[ (57 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( ( D & E ) | ( A & ( D | E ) ) ) + 0x8F1BBCDCL + ( data[ 58 & 15 ] = (((( data[ 58 & 15 ] ^ data[ (58 - 14) & 15 ] ^ data[ (58 - 8) & 15 ] ^ data[ (58 - 3) & 15 ] ))<<(1)) | ((( data[ 58 & 15 ] ^ data[ (58 - 14) & 15 ] ^ data[ (58 - 8) & 15 ] ^ data[ (58 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) ); ;
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( ( C & D ) | ( E & ( C | D ) ) ) + 0x8F1BBCDCL + ( data[ 59 & 15 ] = (((( data[ 59 & 15 ] ^ data[ (59 - 14) & 15 ] ^ data[ (59 - 8) & 15 ] ^ data[ (59 - 3) & 15 ] ))<<(1)) | ((( data[ 59 & 15 ] ^ data[ (59 - 14) & 15 ] ^ data[ (59 - 8) & 15 ] ^ data[ (59 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) ); ;

  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0xCA62C1D6L + ( data[ 60 & 15 ] = (((( data[ 60 & 15 ] ^ data[ (60 - 14) & 15 ] ^ data[ (60 - 8) & 15 ] ^ data[ (60 - 3) & 15 ] ))<<(1)) | ((( data[ 60 & 15 ] ^ data[ (60 - 14) & 15 ] ^ data[ (60 - 8) & 15 ] ^ data[ (60 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) ); ;
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0xCA62C1D6L + ( data[ 61 & 15 ] = (((( data[ 61 & 15 ] ^ data[ (61 - 14) & 15 ] ^ data[ (61 - 8) & 15 ] ^ data[ (61 - 3) & 15 ] ))<<(1)) | ((( data[ 61 & 15 ] ^ data[ (61 - 14) & 15 ] ^ data[ (61 - 8) & 15 ] ^ data[ (61 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) ); ;
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0xCA62C1D6L + ( data[ 62 & 15 ] = (((( data[ 62 & 15 ] ^ data[ (62 - 14) & 15 ] ^ data[ (62 - 8) & 15 ] ^ data[ (62 - 3) & 15 ] ))<<(1)) | ((( data[ 62 & 15 ] ^ data[ (62 - 14) & 15 ] ^ data[ (62 - 8) & 15 ] ^ data[ (62 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0xCA62C1D6L + ( data[ 63 & 15 ] = (((( data[ 63 & 15 ] ^ data[ (63 - 14) & 15 ] ^ data[ (63 - 8) & 15 ] ^ data[ (63 - 3) & 15 ] ))<<(1)) | ((( data[ 63 & 15 ] ^ data[ (63 - 14) & 15 ] ^ data[ (63 - 8) & 15 ] ^ data[ (63 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0xCA62C1D6L + ( data[ 64 & 15 ] = (((( data[ 64 & 15 ] ^ data[ (64 - 14) & 15 ] ^ data[ (64 - 8) & 15 ] ^ data[ (64 - 3) & 15 ] ))<<(1)) | ((( data[ 64 & 15 ] ^ data[ (64 - 14) & 15 ] ^ data[ (64 - 8) & 15 ] ^ data[ (64 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0xCA62C1D6L + ( data[ 65 & 15 ] = (((( data[ 65 & 15 ] ^ data[ (65 - 14) & 15 ] ^ data[ (65 - 8) & 15 ] ^ data[ (65 - 3) & 15 ] ))<<(1)) | ((( data[ 65 & 15 ] ^ data[ (65 - 14) & 15 ] ^ data[ (65 - 8) & 15 ] ^ data[ (65 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0xCA62C1D6L + ( data[ 66 & 15 ] = (((( data[ 66 & 15 ] ^ data[ (66 - 14) & 15 ] ^ data[ (66 - 8) & 15 ] ^ data[ (66 - 3) & 15 ] ))<<(1)) | ((( data[ 66 & 15 ] ^ data[ (66 - 14) & 15 ] ^ data[ (66 - 8) & 15 ] ^ data[ (66 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0xCA62C1D6L + ( data[ 67 & 15 ] = (((( data[ 67 & 15 ] ^ data[ (67 - 14) & 15 ] ^ data[ (67 - 8) & 15 ] ^ data[ (67 - 3) & 15 ] ))<<(1)) | ((( data[ 67 & 15 ] ^ data[ (67 - 14) & 15 ] ^ data[ (67 - 8) & 15 ] ^ data[ (67 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0xCA62C1D6L + ( data[ 68 & 15 ] = (((( data[ 68 & 15 ] ^ data[ (68 - 14) & 15 ] ^ data[ (68 - 8) & 15 ] ^ data[ (68 - 3) & 15 ] ))<<(1)) | ((( data[ 68 & 15 ] ^ data[ (68 - 14) & 15 ] ^ data[ (68 - 8) & 15 ] ^ data[ (68 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0xCA62C1D6L + ( data[ 69 & 15 ] = (((( data[ 69 & 15 ] ^ data[ (69 - 14) & 15 ] ^ data[ (69 - 8) & 15 ] ^ data[ (69 - 3) & 15 ] ))<<(1)) | ((( data[ 69 & 15 ] ^ data[ (69 - 14) & 15 ] ^ data[ (69 - 8) & 15 ] ^ data[ (69 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0xCA62C1D6L + ( data[ 70 & 15 ] = (((( data[ 70 & 15 ] ^ data[ (70 - 14) & 15 ] ^ data[ (70 - 8) & 15 ] ^ data[ (70 - 3) & 15 ] ))<<(1)) | ((( data[ 70 & 15 ] ^ data[ (70 - 14) & 15 ] ^ data[ (70 - 8) & 15 ] ^ data[ (70 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0xCA62C1D6L + ( data[ 71 & 15 ] = (((( data[ 71 & 15 ] ^ data[ (71 - 14) & 15 ] ^ data[ (71 - 8) & 15 ] ^ data[ (71 - 3) & 15 ] ))<<(1)) | ((( data[ 71 & 15 ] ^ data[ (71 - 14) & 15 ] ^ data[ (71 - 8) & 15 ] ^ data[ (71 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0xCA62C1D6L + ( data[ 72 & 15 ] = (((( data[ 72 & 15 ] ^ data[ (72 - 14) & 15 ] ^ data[ (72 - 8) & 15 ] ^ data[ (72 - 3) & 15 ] ))<<(1)) | ((( data[ 72 & 15 ] ^ data[ (72 - 14) & 15 ] ^ data[ (72 - 8) & 15 ] ^ data[ (72 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0xCA62C1D6L + ( data[ 73 & 15 ] = (((( data[ 73 & 15 ] ^ data[ (73 - 14) & 15 ] ^ data[ (73 - 8) & 15 ] ^ data[ (73 - 3) & 15 ] ))<<(1)) | ((( data[ 73 & 15 ] ^ data[ (73 - 14) & 15 ] ^ data[ (73 - 8) & 15 ] ^ data[ (73 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) );
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0xCA62C1D6L + ( data[ 74 & 15 ] = (((( data[ 74 & 15 ] ^ data[ (74 - 14) & 15 ] ^ data[ (74 - 8) & 15 ] ^ data[ (74 - 3) & 15 ] ))<<(1)) | ((( data[ 74 & 15 ] ^ data[ (74 - 14) & 15 ] ^ data[ (74 - 8) & 15 ] ^ data[ (74 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) );
  ( E += (((A)<<(5)) | ((A)>>(32-(5)))) + ( B ^ C ^ D ) + 0xCA62C1D6L + ( data[ 75 & 15 ] = (((( data[ 75 & 15 ] ^ data[ (75 - 14) & 15 ] ^ data[ (75 - 8) & 15 ] ^ data[ (75 - 3) & 15 ] ))<<(1)) | ((( data[ 75 & 15 ] ^ data[ (75 - 14) & 15 ] ^ data[ (75 - 8) & 15 ] ^ data[ (75 - 3) & 15 ] ))>>(32-(1)))) ), B = (((B)<<(30)) | ((B)>>(32-(30)))) );
  ( D += (((E)<<(5)) | ((E)>>(32-(5)))) + ( A ^ B ^ C ) + 0xCA62C1D6L + ( data[ 76 & 15 ] = (((( data[ 76 & 15 ] ^ data[ (76 - 14) & 15 ] ^ data[ (76 - 8) & 15 ] ^ data[ (76 - 3) & 15 ] ))<<(1)) | ((( data[ 76 & 15 ] ^ data[ (76 - 14) & 15 ] ^ data[ (76 - 8) & 15 ] ^ data[ (76 - 3) & 15 ] ))>>(32-(1)))) ), A = (((A)<<(30)) | ((A)>>(32-(30)))) );
  ( C += (((D)<<(5)) | ((D)>>(32-(5)))) + ( E ^ A ^ B ) + 0xCA62C1D6L + ( data[ 77 & 15 ] = (((( data[ 77 & 15 ] ^ data[ (77 - 14) & 15 ] ^ data[ (77 - 8) & 15 ] ^ data[ (77 - 3) & 15 ] ))<<(1)) | ((( data[ 77 & 15 ] ^ data[ (77 - 14) & 15 ] ^ data[ (77 - 8) & 15 ] ^ data[ (77 - 3) & 15 ] ))>>(32-(1)))) ), E = (((E)<<(30)) | ((E)>>(32-(30)))) );
  ( B += (((C)<<(5)) | ((C)>>(32-(5)))) + ( D ^ E ^ A ) + 0xCA62C1D6L + ( data[ 78 & 15 ] = (((( data[ 78 & 15 ] ^ data[ (78 - 14) & 15 ] ^ data[ (78 - 8) & 15 ] ^ data[ (78 - 3) & 15 ] ))<<(1)) | ((( data[ 78 & 15 ] ^ data[ (78 - 14) & 15 ] ^ data[ (78 - 8) & 15 ] ^ data[ (78 - 3) & 15 ] ))>>(32-(1)))) ), D = (((D)<<(30)) | ((D)>>(32-(30)))) ); ;
  ( A += (((B)<<(5)) | ((B)>>(32-(5)))) + ( C ^ D ^ E ) + 0xCA62C1D6L + ( data[ 79 & 15 ] = (((( data[ 79 & 15 ] ^ data[ (79 - 14) & 15 ] ^ data[ (79 - 8) & 15 ] ^ data[ (79 - 3) & 15 ] ))<<(1)) | ((( data[ 79 & 15 ] ^ data[ (79 - 14) & 15 ] ^ data[ (79 - 8) & 15 ] ^ data[ (79 - 3) & 15 ] ))>>(32-(1)))) ), C = (((C)<<(30)) | ((C)>>(32-(30)))) ); ;


  state[0] += A;
  state[1] += B;
  state[2] += C;
  state[3] += D;
  state[4] += E;





}
# 7 "sha1-example.c" 2
# 1 "sha1.c" 1
# 43 "sha1.c"
# 1 "/usr/include/assert.h" 1 3 4
# 64 "/usr/include/assert.h" 3 4



extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



# 44 "sha1.c" 2
# 53 "sha1.c"
void
nettle_sha1_init(struct sha1_ctx *ctx)
{


  static const uint32_t iv[5] =
    {

      0x67452301L,
      0xEFCDAB89L,
      0x98BADCFEL,
      0x10325476L,
      0xC3D2E1F0L,
    };

  memcpy(ctx->state, iv, sizeof(ctx->state));
  ctx->count_low = ctx->count_high = 0;


  ctx->index = 0;
}



void
nettle_sha1_update(struct sha1_ctx *ctx,
     unsigned length, const uint8_t *data)
{
  do { if ((ctx)->index) { unsigned __md_left = sizeof((ctx)->block) - (ctx)->index; if ((length) < __md_left) { memcpy((ctx)->block + (ctx)->index, (data), (length)); (ctx)->index += (length); goto __md_done; } else { memcpy((ctx)->block + (ctx)->index, (data), __md_left); (_nettle_sha1_compress(((ctx))->state, (ctx)->block)); (((ctx)->count_high += !++(ctx)->count_low)); (data) += __md_left; (length) -= __md_left; } } while ((length) >= sizeof((ctx)->block)) { (_nettle_sha1_compress(((ctx))->state, (data))); (((ctx)->count_high += !++(ctx)->count_low)); (data) += sizeof((ctx)->block); (length) -= sizeof((ctx)->block); } memcpy ((ctx)->block, (data), (length)); (ctx)->index = (length); __md_done: ; } while (0);
}

void
nettle_sha1_digest(struct sha1_ctx *ctx,
     unsigned length,
     uint8_t *digest)
{
  uint32_t high, low;

  ((length <= 20) ? (void) (0) : __assert_fail ("length <= 20", "sha1.c", 91, __PRETTY_FUNCTION__));

  do { unsigned __md_i; __md_i = (ctx)->index; ((__md_i < sizeof((ctx)->block)) ? (void) (0) : __assert_fail ("__md_i < sizeof((ctx)->block)", "sha1.c", 93, __PRETTY_FUNCTION__)); (ctx)->block[__md_i++] = 0x80; if (__md_i > (sizeof((ctx)->block) - 2*sizeof((ctx)->count_low))) { memset((ctx)->block + __md_i, 0, sizeof((ctx)->block) - __md_i); (_nettle_sha1_compress(((ctx))->state, (ctx)->block)); __md_i = 0; } memset((ctx)->block + __md_i, 0, sizeof((ctx)->block) - (8) - __md_i); } while (0);


  high = (ctx->count_high << 9) | (ctx->count_low >> 23);
  low = (ctx->count_low << 9) | (ctx->index << 3);


  do { (ctx->block + (64 - 8))[0] = ((high) >> 24) & 0xff; (ctx->block + (64 - 8))[1] = ((high) >> 16) & 0xff; (ctx->block + (64 - 8))[2] = ((high) >> 8) & 0xff; (ctx->block + (64 - 8))[3] = (high) & 0xff; } while(0);
  do { (ctx->block + (64 - 4))[0] = ((low) >> 24) & 0xff; (ctx->block + (64 - 4))[1] = ((low) >> 16) & 0xff; (ctx->block + (64 - 4))[2] = ((low) >> 8) & 0xff; (ctx->block + (64 - 4))[3] = (low) & 0xff; } while(0);
  _nettle_sha1_compress(ctx->state, ctx->block);

  _nettle_write_be32(length, digest, ctx->state);
  nettle_sha1_init(ctx);
}
# 8 "sha1-example.c" 2

struct sha1_ctx ctx;
uint8_t input[3] = { 'a', 'b', 'c' };
uint8_t digest[20];




int main() {
  nettle_sha1_init(&ctx);
  nettle_sha1_update(&ctx, 3, input);
  nettle_sha1_digest(&ctx, 20, digest);
# 29 "sha1-example.c"
  return 0;
}
