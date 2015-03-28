#define __NR_write 1
#define __NR_exit 60

static __inline long long __syscall0(long long n)
{
	unsigned long long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n) : "rcx", "r11", "memory");
	return ret;
}

static __inline long long __syscall1(long long n, long long a1)
{
	unsigned long long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1) : "rcx", "r11", "memory");
	return ret;
}

static __inline long long __syscall2(long long n, long long a1, long long a2)
{
	unsigned long long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2)
						  : "rcx", "r11", "memory");
	return ret;
}

static __inline long long __syscall3(long long n, long long a1, long long a2, long long a3)
{
	unsigned long long ret;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
						  "rdx"(a3) : "rcx", "r11", "memory");
	return ret;
}

static __inline long long __syscall4(long long n, long long a1, long long a2, long long a3, long long a4)
{
	unsigned long long ret;
	register long long r10 __asm__("r10") = a4;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
						  "d"(a3), "r"(r10): "rcx", "r11", "memory");
	return ret;
}

static __inline long long __syscall5(long long n, long long a1, long long a2, long long a3, long long a4, long long a5)
{
	unsigned long long ret;
	register long long r10 __asm__("r10") = a4;
	register long long r8 __asm__("r8") = a5;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
						  "d"(a3), "r"(r10), "r"(r8) : "rcx", "r11", "memory");
	return ret;
}

static __inline long long __syscall6(long long n, long long a1, long long a2, long long a3, long long a4, long long a5, long long a6)
{
	unsigned long long ret;
	register long long r10 __asm__("r10") = a4;
	register long long r8 __asm__("r8") = a5;
	register long long r9 __asm__("r9") = a6;
	__asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(n), "D"(a1), "S"(a2),
						  "d"(a3), "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
	return ret;
}