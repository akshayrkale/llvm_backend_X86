/*
 * This file was generated automatically by xsubpp version 1.9508 from the
 * contents of Glob.xs. Do not edit this file, edit Glob.xs instead.
 *
 *	ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "Glob.xs"
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "bsd_glob.h"

#define MY_CXT_KEY "File::Glob::_guts" XS_VERSION

typedef struct {
    int		x_GLOB_ERROR;
} my_cxt_t;

START_MY_CXT

#define GLOB_ERROR	(MY_CXT.x_GLOB_ERROR)

#include "const-c.inc"

#ifdef WIN32
#define errfunc		NULL
#else
int
errfunc(const char *foo, int bar) {
  return !(bar == EACCES || bar == ENOENT || bar == ENOTDIR);
}
#endif

#line 38 "Glob.c"
XS(XS_File__Glob_doglob); /* prototype to pass -Wmissing-prototypes */
XS(XS_File__Glob_doglob)
{
    dXSARGS;
    if (items < 1)
	Perl_croak(aTHX_ "Usage: File::Glob::doglob(pattern, ...)");
    SP -= items;
    {
	char *	pattern = (char *)SvPV_nolen(ST(0));
#line 40 "Glob.xs"
    glob_t pglob;
    int i;
    int retval;
    int flags = 0;
    SV *tmp;
#line 54 "Glob.c"
#line 46 "Glob.xs"
    {
	dMY_CXT;

	/* allow for optional flags argument */
	if (items > 1) {
	    flags = (int) SvIV(ST(1));
	}

	/* call glob */
	retval = bsd_glob(pattern, flags, errfunc, &pglob);
	GLOB_ERROR = retval;

	/* return any matches found */
	EXTEND(sp, pglob.gl_pathc);
	for (i = 0; i < pglob.gl_pathc; i++) {
	    /* printf("# bsd_glob: %s\n", pglob.gl_pathv[i]); */
	    tmp = sv_2mortal(newSVpvn(pglob.gl_pathv[i],
				      strlen(pglob.gl_pathv[i])));
	    TAINT;
	    SvTAINT(tmp);
	    PUSHs(tmp);
	}

	bsd_globfree(&pglob);
    }
#line 81 "Glob.c"
	PUTBACK;
	return;
    }
}


/* INCLUDE:  Including 'const-xs.inc' from 'Glob.xs' */

XS(XS_File__Glob_constant); /* prototype to pass -Wmissing-prototypes */
XS(XS_File__Glob_constant)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: File::Glob::constant(sv)");
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
#line 109 "Glob.c"
	SV *	sv = ST(0);
	const char *	s = SvPV(sv, len);
#line 18 "const-xs.inc"
        /* Change this to constant(aTHX_ s, len, &iv, &nv);
           if you need to return both NVs and IVs */
	type = constant(aTHX_ s, len, &iv);
      /* Return 1 or 2 items. First is error message, or undef if no error.
           Second, if present, is found value */
        switch (type) {
        case PERL_constant_NOTFOUND:
          sv = sv_2mortal(newSVpvf("%s is not a valid File::Glob macro", s));
          PUSHs(sv);
          break;
        case PERL_constant_NOTDEF:
          sv = sv_2mortal(newSVpvf(
	    "Your vendor has not defined File::Glob macro %s, used", s));
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
	/* Uncomment this if you need to return SVs
        case PERL_constant_ISSV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(sv);
          break; */
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
	    "Unexpected return type %d while processing File::Glob macro %s, used",
               type, s));
          PUSHs(sv);
        }
#line 184 "Glob.c"
	PUTBACK;
	return;
    }
}


/* INCLUDE: Returning to 'Glob.xs' from 'const-xs.inc' */

#ifdef __cplusplus
extern "C"
#endif
XS(boot_File__Glob); /* prototype to pass -Wmissing-prototypes */
XS(boot_File__Glob)
{
    dXSARGS;
    char* file = __FILE__;

    XS_VERSION_BOOTCHECK ;

        newXSproto("File::Glob::doglob", XS_File__Glob_doglob, file, "$;$");
        newXS("File::Glob::constant", XS_File__Glob_constant, file);

    /* Initialisation Section */

#line 31 "Glob.xs"
{
    MY_CXT_INIT;
}

#line 214 "Glob.c"

    /* End of Initialisation Section */

    XSRETURN_YES;
}
