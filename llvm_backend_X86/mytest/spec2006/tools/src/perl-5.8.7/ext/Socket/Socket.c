/*
 * This file was generated automatically by xsubpp version 1.9508 from the
 * contents of Socket.xs. Do not edit this file, edit Socket.xs instead.
 *
 *	ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "Socket.xs"
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <stddef.h>

#ifndef VMS
# ifdef I_SYS_TYPES
#  include <sys/types.h>
# endif
# if !defined(ultrix) /* Avoid double definition. */
#   include <sys/socket.h>
# endif
# if defined(USE_SOCKS) && defined(I_SOCKS)
#   include <socks.h>
# endif
# ifdef MPE
#  define PF_INET AF_INET
#  define PF_UNIX AF_UNIX
#  define SOCK_RAW 3
# endif
# ifdef I_SYS_UN
#  include <sys/un.h>
# endif
/* XXX Configure test for <netinet/in_systm.h needed XXX */
# if defined(NeXT) || defined(__NeXT__)
#  include <netinet/in_systm.h>
# endif
# if defined(__sgi) && !defined(AF_LINK) && defined(PF_LINK) && PF_LINK == AF_LNK
#  undef PF_LINK
# endif
# if defined(I_NETINET_IN) || defined(__ultrix__)
#  include <netinet/in.h>
# endif
# ifdef I_NETDB
#  if !defined(ultrix)  /* Avoid double definition. */
#   include <netdb.h>
#  endif
# endif
# ifdef I_ARPA_INET
#  include <arpa/inet.h>
# endif
# ifdef I_NETINET_TCP
#  include <netinet/tcp.h>
# endif
#else
# include "sockadapt.h"
#endif

#ifdef NETWARE
NETDB_DEFINE_CONTEXT
NETINET_DEFINE_CONTEXT
#endif

#ifdef I_SYSUIO
# include <sys/uio.h>
#endif

#ifndef AF_NBS
# undef PF_NBS
#endif

#ifndef AF_X25
# undef PF_X25
#endif

#ifndef INADDR_NONE
# define INADDR_NONE	0xffffffff
#endif /* INADDR_NONE */
#ifndef INADDR_BROADCAST
# define INADDR_BROADCAST	0xffffffff
#endif /* INADDR_BROADCAST */
#ifndef INADDR_LOOPBACK
# define INADDR_LOOPBACK         0x7F000001
#endif /* INADDR_LOOPBACK */

#ifndef HAS_INET_ATON

/*
 * Check whether "cp" is a valid ascii representation
 * of an Internet address and convert to a binary address.
 * Returns 1 if the address is valid, 0 if not.
 * This replaces inet_addr, the return value from which
 * cannot distinguish between failure and a local broadcast address.
 */
static int
my_inet_aton(register const char *cp, struct in_addr *addr)
{
	dTHX;
	register U32 val;
	register int base;
	register char c;
	int nparts;
	const char *s;
	unsigned int parts[4];
	register unsigned int *pp = parts;

       if (!cp || !*cp)
		return 0;
	for (;;) {
		/*
		 * Collect number up to ``.''.
		 * Values are specified as for C:
		 * 0x=hex, 0=octal, other=decimal.
		 */
		val = 0; base = 10;
		if (*cp == '0') {
			if (*++cp == 'x' || *cp == 'X')
				base = 16, cp++;
			else
				base = 8;
		}
		while ((c = *cp) != '\0') {
			if (isDIGIT(c)) {
				val = (val * base) + (c - '0');
				cp++;
				continue;
			}
			if (base == 16 && (s=strchr(PL_hexdigit,c))) {
				val = (val << 4) +
					((s - PL_hexdigit) & 15);
				cp++;
				continue;
			}
			break;
		}
		if (*cp == '.') {
			/*
			 * Internet format:
			 *	a.b.c.d
			 *	a.b.c	(with c treated as 16-bits)
			 *	a.b	(with b treated as 24 bits)
			 */
			if (pp >= parts + 3 || val > 0xff)
				return 0;
			*pp++ = val, cp++;
		} else
			break;
	}
	/*
	 * Check for trailing characters.
	 */
	if (*cp && !isSPACE(*cp))
		return 0;
	/*
	 * Concoct the address according to
	 * the number of parts specified.
	 */
	nparts = pp - parts + 1;	/* force to an int for switch() */
	switch (nparts) {

	case 1:				/* a -- 32 bits */
		break;

	case 2:				/* a.b -- 8.24 bits */
		if (val > 0xffffff)
			return 0;
		val |= parts[0] << 24;
		break;

	case 3:				/* a.b.c -- 8.8.16 bits */
		if (val > 0xffff)
			return 0;
		val |= (parts[0] << 24) | (parts[1] << 16);
		break;

	case 4:				/* a.b.c.d -- 8.8.8.8 bits */
		if (val > 0xff)
			return 0;
		val |= (parts[0] << 24) | (parts[1] << 16) | (parts[2] << 8);
		break;
	}
	addr->s_addr = htonl(val);
	return 1;
}

#undef inet_aton
#define inet_aton my_inet_aton

#endif /* ! HAS_INET_ATON */


static int
not_here(char *s)
{
    croak("Socket::%s not implemented on this architecture", s);
    return -1;
}

#define PERL_IN_ADDR_S_ADDR_SIZE 4

/*
* Bad assumptions possible here.
*
* Bad Assumption 1: struct in_addr has no other fields
* than the s_addr (which is the field we care about
* in here, really). However, we can be fed either 4-byte
* addresses (from pack("N", ...), or va.b.c.d, or ...),
* or full struct in_addrs (from e.g. pack_sockaddr_in()),
* which may or may not be 4 bytes in size.
*
* Bad Assumption 2: the s_addr field is a simple type
* (such as an int, u_int32_t).  It can be a bit field,
* in which case using & (address-of) on it or taking sizeof()
* wouldn't go over too well.  (Those are not attempted
* now but in case someone thinks to change the below code
* to use addr.s_addr instead of addr, you have been warned.)
*
* Bad Assumption 3: the s_addr is the first field in
* an in_addr, or that its bytes are the first bytes in
* an in_addr.
*
* These bad assumptions are wrong in UNICOS which has
* struct in_addr { struct { u_long  st_addr:32; } s_da };
* #define s_addr s_da.st_addr
* and u_long is 64 bits.
*
* --jhi */

#include "const-c.inc"

#line 233 "Socket.c"

/* INCLUDE:  Including 'const-xs.inc' from 'Socket.xs' */

XS(XS_Socket_constant); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_constant)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::constant(sv)");
    SP -= items;
    {
#line 4 "const-xs.inc"
#ifdef dXSTARG
	dXSTARG; /* Faster if we have it.  */
#else
	dTARGET;
#endif
	STRLEN		len;
        int		type;
	IV		iv;
	/* NV		nv;	Uncomment this if you need to return NVs */
	/* const char	*pv;	Uncomment this if you need to return PVs */
#line 256 "Socket.c"
	SV *	sv = ST(0);
	const char *	s = SvPV(sv, len);
#line 18 "const-xs.inc"
        /* Change this to constant(aTHX_ s, len, &iv, &nv);
           if you need to return both NVs and IVs */
	type = constant(aTHX_ s, len, &iv, &sv);
      /* Return 1 or 2 items. First is error message, or undef if no error.
           Second, if present, is found value */
        switch (type) {
        case PERL_constant_NOTFOUND:
          sv = sv_2mortal(newSVpvf("%s is not a valid Socket macro", s));
          PUSHs(sv);
          break;
        case PERL_constant_NOTDEF:
          sv = sv_2mortal(newSVpvf(
	    "Your vendor has not defined Socket macro %s, used", s));
          PUSHs(sv);
          break;
        case PERL_constant_ISIV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHi(iv);
          break;
	/* Uncomment this if you need to return NOs
        case PERL_constant_ISNO:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(&PL_sv_no);
          break; */
	/* Uncomment this if you need to return NVs
        case PERL_constant_ISNV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHn(nv);
          break; */
	/* Uncomment this if you need to return PVs
        case PERL_constant_ISPV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHp(pv, strlen(pv));
          break; */
	/* Uncomment this if you need to return PVNs
        case PERL_constant_ISPVN:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHp(pv, iv);
          break; */
        case PERL_constant_ISSV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(sv);
          break;
	/* Uncomment this if you need to return UNDEFs
        case PERL_constant_ISUNDEF:
          break; */
	/* Uncomment this if you need to return UVs
        case PERL_constant_ISUV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHu((UV)iv);
          break; */
	/* Uncomment this if you need to return YESs
        case PERL_constant_ISYES:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(&PL_sv_yes);
          break; */
        default:
          sv = sv_2mortal(newSVpvf(
	    "Unexpected return type %d while processing Socket macro %s, used",
               type, s));
          PUSHs(sv);
        }
#line 330 "Socket.c"
	PUTBACK;
	return;
    }
}


/* INCLUDE: Returning to 'Socket.xs' from 'const-xs.inc' */

XS(XS_Socket_inet_aton); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_inet_aton)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::inet_aton(host)");
    {
	char *	host = (char *)SvPV_nolen(ST(0));
#line 231 "Socket.xs"
	{
	struct in_addr ip_address;
	struct hostent * phe;
	int ok =
		(host != NULL) &&
		(*host != '\0') &&
		inet_aton(host, &ip_address);

	if (!ok && (phe = gethostbyname(host))) {
		Copy( phe->h_addr, &ip_address, phe->h_length, char );
		ok = 1;
	}

	ST(0) = sv_newmortal();
	if (ok)
		sv_setpvn( ST(0), (char *)&ip_address, sizeof ip_address );
	}
#line 365 "Socket.c"
    }
    XSRETURN(1);
}

XS(XS_Socket_inet_ntoa); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_inet_ntoa)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::inet_ntoa(ip_address_sv)");
    {
	SV *	ip_address_sv = ST(0);
#line 253 "Socket.xs"
	{
	STRLEN addrlen;
	struct in_addr addr;
	char * addr_str;
	char * ip_address;
	if (DO_UTF8(ip_address_sv) && !sv_utf8_downgrade(ip_address_sv, 1))
	     croak("Wide character in Socket::inet_ntoa");
	ip_address = SvPVbyte(ip_address_sv, addrlen);
	if (addrlen == sizeof(addr) || addrlen == 4)
	        addr.s_addr =
		    (ip_address[0] & 0xFF) << 24 |
		    (ip_address[1] & 0xFF) << 16 |
		    (ip_address[2] & 0xFF) <<  8 |
		    (ip_address[3] & 0xFF);
	else
	        croak("Bad arg length for %s, length is %d, should be %d",
		      "Socket::inet_ntoa",
		      addrlen, sizeof(addr));
	/* We could use inet_ntoa() but that is broken
	 * in HP-UX + GCC + 64bitint (returns "0.0.0.0"),
	 * so let's use this sprintf() workaround everywhere.
	 * This is also more threadsafe than using inet_ntoa(). */
	New(1138, addr_str, 4 * 3 + 3 + 1, char); /* IPv6? */
	sprintf(addr_str, "%d.%d.%d.%d",
		((addr.s_addr >> 24) & 0xFF),
		((addr.s_addr >> 16) & 0xFF),
		((addr.s_addr >>  8) & 0xFF),
		( addr.s_addr        & 0xFF));
	ST(0) = sv_2mortal(newSVpvn(addr_str, strlen(addr_str)));
	Safefree(addr_str);
	}
#line 410 "Socket.c"
    }
    XSRETURN(1);
}

XS(XS_Socket_sockaddr_family); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_sockaddr_family)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::sockaddr_family(sockaddr)");
    {
	SV *	sockaddr = ST(0);
#line 289 "Socket.xs"
	STRLEN sockaddr_len;
	char *sockaddr_pv = SvPVbyte(sockaddr, sockaddr_len);
#line 426 "Socket.c"
#line 292 "Socket.xs"
	if (sockaddr_len < offsetof(struct sockaddr, sa_data)) {
	    croak("Bad arg length for %s, length is %d, should be at least %d",
	          "Socket::sockaddr_family", sockaddr_len,
		  offsetof(struct sockaddr, sa_data));
	}
	ST(0) = sv_2mortal(newSViv(((struct sockaddr*)sockaddr_pv)->sa_family));
#line 434 "Socket.c"
    }
    XSRETURN(1);
}

XS(XS_Socket_pack_sockaddr_un); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_pack_sockaddr_un)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::pack_sockaddr_un(pathname)");
    {
	SV *	pathname = ST(0);
#line 303 "Socket.xs"
	{
#ifdef I_SYS_UN
	struct sockaddr_un sun_ad; /* fear using sun */
	STRLEN len;
	char * pathname_pv;

	Zero( &sun_ad, sizeof sun_ad, char );
	sun_ad.sun_family = AF_UNIX;
	pathname_pv = SvPV(pathname,len);
	if (len > sizeof(sun_ad.sun_path))
	    len = sizeof(sun_ad.sun_path);
#  ifdef OS2	/* Name should start with \socket\ and contain backslashes! */
	{
	    int off;
	    char *s, *e;

	    if (pathname_pv[0] != '/' && pathname_pv[0] != '\\')
		croak("Relative UNIX domain socket name '%s' unsupported",
			pathname_pv);
	    else if (len < 8
		     || pathname_pv[7] != '/' && pathname_pv[7] != '\\'
		     || !strnicmp(pathname_pv + 1, "socket", 6))
		off = 7;
	    else
		off = 0;		/* Preserve names starting with \socket\ */
	    Copy( "\\socket", sun_ad.sun_path, off, char);
	    Copy( pathname_pv, sun_ad.sun_path + off, len, char );

	    s = sun_ad.sun_path + off - 1;
	    e = s + len + 1;
	    while (++s < e)
		if (*s = '/')
		    *s = '\\';
	}
#  else	/* !( defined OS2 ) */
	Copy( pathname_pv, sun_ad.sun_path, len, char );
#  endif
	if (0) not_here("dummy");
	ST(0) = sv_2mortal(newSVpvn((char *)&sun_ad, sizeof sun_ad));
#else
	ST(0) = (SV *) not_here("pack_sockaddr_un");
#endif

	}
#line 492 "Socket.c"
    }
    XSRETURN(1);
}

XS(XS_Socket_unpack_sockaddr_un); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_unpack_sockaddr_un)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::unpack_sockaddr_un(sun_sv)");
    {
	SV *	sun_sv = ST(0);
#line 352 "Socket.xs"
	{
#ifdef I_SYS_UN
	struct sockaddr_un addr;
	STRLEN sockaddrlen;
	char * sun_ad = SvPVbyte(sun_sv,sockaddrlen);
	char * e;
#   ifndef __linux__
	/* On Linux sockaddrlen on sockets returned by accept, recvfrom,
	   getpeername and getsockname is not equal to sizeof(addr). */
	if (sockaddrlen != sizeof(addr)) {
	    croak("Bad arg length for %s, length is %d, should be %d",
			"Socket::unpack_sockaddr_un",
			sockaddrlen, sizeof(addr));
	}
#   endif

	Copy( sun_ad, &addr, sizeof addr, char );

	if ( addr.sun_family != AF_UNIX ) {
	    croak("Bad address family for %s, got %d, should be %d",
			"Socket::unpack_sockaddr_un",
			addr.sun_family,
			AF_UNIX);
	}
	e = (char*)addr.sun_path;
	/* On Linux, the name of abstract unix domain sockets begins
	 * with a '\0', so allow this. */
	while ((*e || (e == addr.sun_path && e[1] && sockaddrlen > 1))
		&& e < (char*)addr.sun_path + sizeof addr.sun_path)
	    ++e;
	ST(0) = sv_2mortal(newSVpvn(addr.sun_path, e - (char*)addr.sun_path));
#else
	ST(0) = (SV *) not_here("unpack_sockaddr_un");
#endif
	}
#line 541 "Socket.c"
    }
    XSRETURN(1);
}

XS(XS_Socket_pack_sockaddr_in); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_pack_sockaddr_in)
{
    dXSARGS;
    if (items != 2)
	Perl_croak(aTHX_ "Usage: Socket::pack_sockaddr_in(port, ip_address_sv)");
    {
	unsigned short	port = (unsigned short)SvUV(ST(0));
	SV *	ip_address_sv = ST(1);
#line 393 "Socket.xs"
	{
	struct sockaddr_in sin;
	struct in_addr addr;
	STRLEN addrlen;
	char * ip_address;
	if (DO_UTF8(ip_address_sv) && !sv_utf8_downgrade(ip_address_sv, 1))
	     croak("Wide character in Socket::pack_sockaddr_in");
	ip_address = SvPVbyte(ip_address_sv, addrlen);
	if (addrlen == sizeof(addr) || addrlen == 4)
	        addr.s_addr =
		    (ip_address[0] & 0xFF) << 24 |
		    (ip_address[1] & 0xFF) << 16 |
		    (ip_address[2] & 0xFF) <<  8 |
		    (ip_address[3] & 0xFF);
	else
	        croak("Bad arg length for %s, length is %d, should be %d",
		      "Socket::pack_sockaddr_in",
		      addrlen, sizeof(addr));
	Zero( &sin, sizeof sin, char );
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);
	sin.sin_addr.s_addr = htonl(addr.s_addr);
	ST(0) = sv_2mortal(newSVpvn((char *)&sin, sizeof sin));
	}
#line 580 "Socket.c"
    }
    XSRETURN(1);
}

XS(XS_Socket_unpack_sockaddr_in); /* prototype to pass -Wmissing-prototypes */
XS(XS_Socket_unpack_sockaddr_in)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Socket::unpack_sockaddr_in(sin_sv)");
    SP -= items;
    {
	SV *	sin_sv = ST(0);
#line 422 "Socket.xs"
	{
	STRLEN sockaddrlen;
	struct sockaddr_in addr;
	unsigned short	port;
	struct in_addr  ip_address;
	char *	sin = SvPVbyte(sin_sv,sockaddrlen);
	if (sockaddrlen != sizeof(addr)) {
	    croak("Bad arg length for %s, length is %d, should be %d",
			"Socket::unpack_sockaddr_in",
			sockaddrlen, sizeof(addr));
	}
	Copy( sin, &addr,sizeof addr, char );
	if ( addr.sin_family != AF_INET ) {
	    croak("Bad address family for %s, got %d, should be %d",
			"Socket::unpack_sockaddr_in",
			addr.sin_family,
			AF_INET);
	}
	port = ntohs(addr.sin_port);
	ip_address = addr.sin_addr;

	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSViv((IV) port)));
	PUSHs(sv_2mortal(newSVpvn((char *)&ip_address, sizeof ip_address)));
	}
#line 620 "Socket.c"
	PUTBACK;
	return;
    }
}

#ifdef __cplusplus
extern "C"
#endif
XS(boot_Socket); /* prototype to pass -Wmissing-prototypes */
XS(boot_Socket)
{
    dXSARGS;
    char* file = __FILE__;

    XS_VERSION_BOOTCHECK ;

        newXS("Socket::constant", XS_Socket_constant, file);
        newXS("Socket::inet_aton", XS_Socket_inet_aton, file);
        newXS("Socket::inet_ntoa", XS_Socket_inet_ntoa, file);
        newXS("Socket::sockaddr_family", XS_Socket_sockaddr_family, file);
        newXS("Socket::pack_sockaddr_un", XS_Socket_pack_sockaddr_un, file);
        newXS("Socket::unpack_sockaddr_un", XS_Socket_unpack_sockaddr_un, file);
        newXS("Socket::pack_sockaddr_in", XS_Socket_pack_sockaddr_in, file);
        newXS("Socket::unpack_sockaddr_in", XS_Socket_unpack_sockaddr_in, file);
    XSRETURN_YES;
}

