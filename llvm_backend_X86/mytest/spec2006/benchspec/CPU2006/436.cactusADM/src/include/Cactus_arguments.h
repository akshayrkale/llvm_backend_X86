#ifdef FCODE
#define DECLARE_CACTUS_PRIVATE_FARGUMENTS \


#define CACTUS_PRIVATE_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CACTUS_PRIVATE_CARGUMENTS \


#define USE_CACTUS_PRIVATE_CARGUMENTS \


#define DECLARE_CACTUS_PRIVATE_C2F \


#define INITIALISE_CACTUS_PRIVATE_C2F \


#define CACTUS_PRIVATE_C2F_PROTO \



#define PASS_CACTUS_PRIVATE_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_CACTUS_PROTECTED_FARGUMENTS \


#define CACTUS_PROTECTED_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CACTUS_PROTECTED_CARGUMENTS \


#define USE_CACTUS_PROTECTED_CARGUMENTS \


#define DECLARE_CACTUS_PROTECTED_C2F \


#define INITIALISE_CACTUS_PROTECTED_C2F \


#define CACTUS_PROTECTED_C2F_PROTO \



#define PASS_CACTUS_PROTECTED_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define DECLARE_CACTUS_PUBLIC_FARGUMENTS \


#define CACTUS_PUBLIC_FARGUMENTS \



#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CACTUS_PUBLIC_CARGUMENTS \


#define USE_CACTUS_PUBLIC_CARGUMENTS \


#define DECLARE_CACTUS_PUBLIC_C2F \


#define INITIALISE_CACTUS_PUBLIC_C2F \


#define CACTUS_PUBLIC_C2F_PROTO \



#define PASS_CACTUS_PUBLIC_C2F(xGH) \



#endif /*CCODE*/


#ifdef FCODE
#define CACTUS_FARGUMENTS _CCTK_FARGUMENTS\


#define DECLARE_CACTUS_FARGUMENTS _DECLARE_CCTK_FARGUMENTS \


#endif /*FCODE*/


#ifdef CCODE
#define DECLARE_CACTUS_CARGUMENTS _DECLARE_CCTK_CARGUMENTS \


#define USE_CACTUS_CARGUMENTS _USE_CCTK_CARGUMENTS \


#endif /*CCODE*/


#ifdef CCODE
#define CACTUS_C2F_PROTO _CCTK_C2F_PROTO\


#define PASS_CACTUS_C2F(xGH) _PASS_CCTK_C2F(xGH)\


#define DECLARE_CACTUS_C2F _DECLARE_CCTK_C2F \


#define INITIALISE_CACTUS_C2F _INITIALISE_CCTK_C2F \


#define CACTUS_CARGUMENTS cGH *cctkGH 


#endif /*CCODE*/


