################################################################################
#
#            !!!!!   Do NOT edit this file directly!   !!!!!
#
#            Edit mktests.PL and/or parts/inc/sv_xpvf instead.
#
################################################################################

BEGIN {
  if ($ENV{'PERL_CORE'}) {
    chdir 't' if -d 't';
    @INC = ('../lib', '../ext/Devel/PPPort/t') if -d '../lib' && -d '../ext';
    require Config; import Config;
    use vars '%Config';
    if (" $Config{'extensions'} " !~ m[ Devel/PPPort ]) {
      print "1..0 # Skip -- Perl configured without Devel::PPPort module\n";
      exit 0;
    }
  }
  else {
    unshift @INC, 't';
  }

  eval "use Test";
  if ($@) {
    require 'testutil.pl';
    print "1..9\n";
  }
  else {
    plan(tests => 9);
  }
}

use Devel::PPPort;
use strict;
$^W = 1;

use Tie::Hash;
my %h;
tie %h, 'Tie::StdHash';
$h{foo} = 'foo-';
$h{bar} = '';

ok(&Devel::PPPort::vnewSVpvf(), $] >= 5.004 ? 'Perl-42' : '%s-%d');
ok(&Devel::PPPort::sv_vcatpvf('1-2-3-'), $] >= 5.004 ? '1-2-3-Perl-42' : '1-2-3-%s-%d');
ok(&Devel::PPPort::sv_vsetpvf('1-2-3-'), $] >= 5.004 ? 'Perl-42' : '%s-%d');

&Devel::PPPort::sv_catpvf_mg($h{foo});
ok($h{foo}, $] >= 5.004 ? 'foo-Perl-42' : 'foo-');

&Devel::PPPort::Perl_sv_catpvf_mg($h{foo});
ok($h{foo}, $] >= 5.004 ? 'foo-Perl-42-Perl-43' : 'foo-');

&Devel::PPPort::sv_catpvf_mg_nocontext($h{foo});
ok($h{foo}, $] >= 5.004 ? 'foo-Perl-42-Perl-43-Perl-44' : 'foo-');

&Devel::PPPort::sv_setpvf_mg($h{bar});
ok($h{bar}, $] >= 5.004 ? 'mhx-42' : '');

&Devel::PPPort::Perl_sv_setpvf_mg($h{bar});
ok($h{bar}, $] >= 5.004 ? 'foo-43' : '');

&Devel::PPPort::sv_setpvf_mg_nocontext($h{bar});
ok($h{bar}, $] >= 5.004 ? 'bar-44' : '');

