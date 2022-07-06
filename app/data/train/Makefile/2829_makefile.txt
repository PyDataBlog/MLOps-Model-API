MAKEFLAGS=-s --no-print-directory
am__is_gnu_make = { \
  if test -z '$(MAKELEVEL)'; then \
    false; \
  elif test -n '$(MAKE_HOST)'; then \
    true; \
  elif test -n '$(MAKE_VERSION)' && test -n '$(CURDIR)'; then \
    true; \
  else \
    false; \
  fi; \
}
am__make_running_with_option = \
  case $${target_option-} in \
      ?) ;; \
      *) echo "am__make_running_with_option: internal error: invalid" \
              "target option '$${target_option-}' specified" >&2; \
         exit 1;; \
  esac; \
  has_opt=no; \
  sane_makeflags=$$MAKEFLAGS; \
  if $(am__is_gnu_make); then \
    sane_makeflags=$$MFLAGS; \
  else \
    case $$MAKEFLAGS in \
      *\\[\ \	]*) \
        bs=\\; \
        sane_makeflags=`printf '%s\n' "$$MAKEFLAGS" \
          | sed "s/$$bs$$bs[$$bs $$bs	]*//g"`;; \
    esac; \
  fi; \
  skip_next=no; \
  strip_trailopt () \
  { \
    flg=`printf '%s\n' "$$flg" | sed "s/$$1.*$$//"`; \
  }; \
  for flg in $$sane_makeflags; do \
    test $$skip_next = yes && { skip_next=no; continue; }; \
    case $$flg in \
      *=*|--*) continue;; \
        -*I) strip_trailopt 'I'; skip_next=yes;; \
      -*I?*) strip_trailopt 'I';; \
        -*O) strip_trailopt 'O'; skip_next=yes;; \
      -*O?*) strip_trailopt 'O';; \
        -*l) strip_trailopt 'l'; skip_next=yes;; \
      -*l?*) strip_trailopt 'l';; \
      -[dEDm]) skip_next=yes;; \
      -[JT]) skip_next=yes;; \
    esac; \
    case $$flg in \
      *$$target_option*) has_opt=yes; break;; \
    esac; \
  done; \
  test $$has_opt = yes
am__make_dryrun = (target_option=n; $(am__make_running_with_option))
am__make_keepgoing = (target_option=k; $(am__make_running_with_option))
pkgdatadir = $(datadir)/amule
pkgincludedir = $(includedir)/amule
pkglibdir = $(libdir)/amule
pkglibexecdir = $(libexecdir)/amule
am__cd = CDPATH="$${ZSH_VERSION+.}$(PATH_SEPARATOR)" && cd
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_SCRIPT = $(install_sh) -c
INSTALL_HEADER = $(INSTALL_DATA)
transform = $(program_transform_name)
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
build_triplet = x86_64-unknown-linux-gnu
host_triplet = x86_64-unknown-linux-gnu
TESTS = CUInt128Test$(EXEEXT) RangeMapTest$(EXEEXT) \
	FormatTest$(EXEEXT) StringFunctionsTest$(EXEEXT) \
	NetworkFunctionsTest$(EXEEXT) FileDataIOTest$(EXEEXT) \
	PathTest$(EXEEXT) TextFileTest$(EXEEXT) CTagTest$(EXEEXT)
check_PROGRAMS = $(am__EXEEXT_1)
subdir = unittests/tests
ACLOCAL_M4 = $(top_srcdir)/aclocal.m4
am__aclocal_m4_deps = $(top_srcdir)/m4/GeoIP.m4 \
	$(top_srcdir)/m4/bfd.m4 $(top_srcdir)/m4/boost.m4 \
	$(top_srcdir)/m4/build-tools.m4 $(top_srcdir)/m4/codeset.m4 \
	$(top_srcdir)/m4/cryptopp.m4 $(top_srcdir)/m4/fallocate.m4 \
	$(top_srcdir)/m4/gdlib.m4 $(top_srcdir)/m4/gettext.m4 \
	$(top_srcdir)/m4/glibc21.m4 $(top_srcdir)/m4/iconv.m4 \
	$(top_srcdir)/m4/intdiv0.m4 $(top_srcdir)/m4/inttypes-pri.m4 \
	$(top_srcdir)/m4/inttypes.m4 $(top_srcdir)/m4/inttypes_h.m4 \
	$(top_srcdir)/m4/isc-posix.m4 $(top_srcdir)/m4/kde.m4 \
	$(top_srcdir)/m4/lcmessage.m4 $(top_srcdir)/m4/lib-ld.m4 \
	$(top_srcdir)/m4/lib-link.m4 $(top_srcdir)/m4/lib-prefix.m4 \
	$(top_srcdir)/m4/libpng.m4 $(top_srcdir)/m4/libupnp.m4 \
	$(top_srcdir)/m4/mmap.m4 $(top_srcdir)/m4/muleboost.m4 \
	$(top_srcdir)/m4/nls.m4 $(top_srcdir)/m4/pkg.m4 \
	$(top_srcdir)/m4/plasmamule.m4 $(top_srcdir)/m4/progtest.m4 \
	$(top_srcdir)/m4/qt.m4 $(top_srcdir)/m4/readline.m4 \
	$(top_srcdir)/m4/stdint_h.m4 $(top_srcdir)/m4/uintmax_t.m4 \
	$(top_srcdir)/m4/ulonglong.m4 $(top_srcdir)/m4/wxwin.m4 \
	$(top_srcdir)/m4/zlib.m4 $(top_srcdir)/acinclude.m4 \
	$(top_srcdir)/configure.in
am__configure_deps = $(am__aclocal_m4_deps) $(CONFIGURE_DEPENDENCIES) \
	$(ACLOCAL_M4)
DIST_COMMON = $(srcdir)/Makefile.am $(am__DIST_COMMON)
mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_HEADER = $(top_builddir)/config.h
CONFIG_CLEAN_FILES =
CONFIG_CLEAN_VPATH_FILES =
am__EXEEXT_1 = CUInt128Test$(EXEEXT) RangeMapTest$(EXEEXT) \
	FormatTest$(EXEEXT) StringFunctionsTest$(EXEEXT) \
	NetworkFunctionsTest$(EXEEXT) FileDataIOTest$(EXEEXT) \
	PathTest$(EXEEXT) TextFileTest$(EXEEXT) CTagTest$(EXEEXT)
am_CTagTest_OBJECTS = CTagTest.$(OBJEXT) SafeFile.$(OBJEXT) \
	MemFile.$(OBJEXT) Tag.$(OBJEXT) Format.$(OBJEXT) \
	strerror_r.$(OBJEXT)
CTagTest_OBJECTS = $(am_CTagTest_OBJECTS)
CTagTest_LDADD = $(LDADD)
am__DEPENDENCIES_1 =
CTagTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_CUInt128Test_OBJECTS = CUInt128Test.$(OBJEXT) UInt128.$(OBJEXT) \
	Format.$(OBJEXT) strerror_r.$(OBJEXT)
CUInt128Test_OBJECTS = $(am_CUInt128Test_OBJECTS)
CUInt128Test_LDADD = $(LDADD)
CUInt128Test_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_FileDataIOTest_OBJECTS = FileDataIOTest.$(OBJEXT) \
	SafeFile.$(OBJEXT) CFile.$(OBJEXT) MemFile.$(OBJEXT) \
	UInt128.$(OBJEXT) StringFunctions.$(OBJEXT) Tag.$(OBJEXT) \
	Path.$(OBJEXT) Format.$(OBJEXT) strerror_r.$(OBJEXT)
FileDataIOTest_OBJECTS = $(am_FileDataIOTest_OBJECTS)
FileDataIOTest_LDADD = $(LDADD)
FileDataIOTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_FormatTest_OBJECTS = FormatTest.$(OBJEXT) Format.$(OBJEXT) \
	strerror_r.$(OBJEXT)
FormatTest_OBJECTS = $(am_FormatTest_OBJECTS)
FormatTest_LDADD = $(LDADD)
FormatTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_NetworkFunctionsTest_OBJECTS =  \
	NetworkFunctionsTest-NetworkFunctionsTest.$(OBJEXT) \
	NetworkFunctionsTest-NetworkFunctions.$(OBJEXT) \
	NetworkFunctionsTest-LibSocket.$(OBJEXT) \
	NetworkFunctionsTest-Format.$(OBJEXT) \
	NetworkFunctionsTest-strerror_r.$(OBJEXT)
NetworkFunctionsTest_OBJECTS = $(am_NetworkFunctionsTest_OBJECTS)
NetworkFunctionsTest_LDADD = $(LDADD)
NetworkFunctionsTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_PathTest_OBJECTS = PathTest.$(OBJEXT) Path.$(OBJEXT) \
	StringFunctions.$(OBJEXT)
PathTest_OBJECTS = $(am_PathTest_OBJECTS)
PathTest_LDADD = $(LDADD)
PathTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_RangeMapTest_OBJECTS = RangeMapTest.$(OBJEXT)
RangeMapTest_OBJECTS = $(am_RangeMapTest_OBJECTS)
RangeMapTest_LDADD = $(LDADD)
RangeMapTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_StringFunctionsTest_OBJECTS = StringFunctionsTest.$(OBJEXT) \
	StringFunctions.$(OBJEXT) Path.$(OBJEXT)
StringFunctionsTest_OBJECTS = $(am_StringFunctionsTest_OBJECTS)
StringFunctionsTest_LDADD = $(LDADD)
StringFunctionsTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am_TextFileTest_OBJECTS = TextFileTest-TextFileTest.$(OBJEXT) \
	TextFileTest-Path.$(OBJEXT) \
	TextFileTest-StringFunctions.$(OBJEXT) \
	TextFileTest-TextFile.$(OBJEXT)
TextFileTest_OBJECTS = $(am_TextFileTest_OBJECTS)
TextFileTest_LDADD = $(LDADD)
TextFileTest_DEPENDENCIES = $(am__DEPENDENCIES_1) \
	../muleunit/libmuleunit.a
am__v_P_1 = :
am__v_GEN_1 = 
am__v_at_1 = 
DEFAULT_INCLUDES = -I. -I$(top_builddir)
depcomp = $(SHELL) $(top_srcdir)/depcomp
am__depfiles_maybe = depfiles
am__mv = mv -f
am__v_lt_1 = 
COMPILE = $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
am__v_CC_1 = 
CCLD = $(CC)
LINK = $(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@
am__v_CCLD_1 = 
CXXCOMPILE = $(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)
am__v_CXX_1 = 
CXXLD = $(CXX)
CXXLINK = $(CXXLD) $(AM_CXXFLAGS) $(CXXFLAGS) $(AM_LDFLAGS) $(LDFLAGS) \
	-o $@
am__v_CXXLD_1 = 
SOURCES = $(CTagTest_SOURCES) $(CUInt128Test_SOURCES) \
	$(FileDataIOTest_SOURCES) $(FormatTest_SOURCES) \
	$(NetworkFunctionsTest_SOURCES) $(PathTest_SOURCES) \
	$(RangeMapTest_SOURCES) $(StringFunctionsTest_SOURCES) \
	$(TextFileTest_SOURCES)
DIST_SOURCES = $(CTagTest_SOURCES) $(CUInt128Test_SOURCES) \
	$(FileDataIOTest_SOURCES) $(FormatTest_SOURCES) \
	$(NetworkFunctionsTest_SOURCES) $(PathTest_SOURCES) \
	$(RangeMapTest_SOURCES) $(StringFunctionsTest_SOURCES) \
	$(TextFileTest_SOURCES)
am__can_run_installinfo = \
  case $$AM_UPDATE_INFO_DIR in \
    n|no|NO) false;; \
    *) (install-info --version) >/dev/null 2>&1;; \
  esac
am__tagged_files = $(HEADERS) $(SOURCES) $(TAGS_FILES) $(LISP)
am__uniquify_input = $(AWK) '\
  BEGIN { nonempty = 0; } \
  { items[$$0] = 1; nonempty = 1; } \
  END { if (nonempty) { for (i in items) print i; }; } \
'
am__define_uniq_tagged_files = \
  list='$(am__tagged_files)'; \
  unique=`for i in $$list; do \
    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
  done | $(am__uniquify_input)`
ETAGS = etags
CTAGS = ctags
am__tty_colors_dummy = \
  mgn= red= grn= lgn= blu= brg= std=; \
  am__color_tests=no
am__tty_colors = { \
  $(am__tty_colors_dummy); \
  if test "X$(AM_COLOR_TESTS)" = Xno; then \
    am__color_tests=no; \
  elif test "X$(AM_COLOR_TESTS)" = Xalways; then \
    am__color_tests=yes; \
  elif test "X$$TERM" != Xdumb && { test -t 1; } 2>/dev/null; then \
    am__color_tests=yes; \
  fi; \
  if test $$am__color_tests = yes; then \
    red='[0;31m'; \
    grn='[0;32m'; \
    lgn='[1;32m'; \
    blu='[1;34m'; \
    mgn='[0;35m'; \
    brg='[1m'; \
    std='[m'; \
  fi; \
}
am__vpath_adj_setup = srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`;
am__vpath_adj = case $$p in \
    $(srcdir)/*) f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`;; \
    *) f=$$p;; \
  esac;
am__strip_dir = f=`echo $$p | sed -e 's|^.*/||'`;
am__install_max = 40
am__nobase_strip_setup = \
  srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*|]/\\\\&/g'`
am__nobase_strip = \
  for p in $$list; do echo "$$p"; done | sed -e "s|$$srcdirstrip/||"
am__nobase_list = $(am__nobase_strip_setup); \
  for p in $$list; do echo "$$p $$p"; done | \
  sed "s| $$srcdirstrip/| |;"' / .*\//!s/ .*/ ./; s,\( .*\)/[^/]*$$,\1,' | \
  $(AWK) 'BEGIN { files["."] = "" } { files[$$2] = files[$$2] " " $$1; \
    if (++n[$$2] == $(am__install_max)) \
      { print $$2, files[$$2]; n[$$2] = 0; files[$$2] = "" } } \
    END { for (dir in files) print dir, files[dir] }'
am__base_list = \
  sed '$$!N;$$!N;$$!N;$$!N;$$!N;$$!N;$$!N;s/\n/ /g' | \
  sed '$$!N;$$!N;$$!N;$$!N;s/\n/ /g'
am__uninstall_files_from_dir = { \
  test -z "$$files" \
    || { test ! -d "$$dir" && test ! -f "$$dir" && test ! -r "$$dir"; } \
    || { echo " ( cd '$$dir' && rm -f" $$files ")"; \
         $(am__cd) "$$dir" && rm -f $$files; }; \
  }
am__recheck_rx = ^[ 	]*:recheck:[ 	]*
am__global_test_result_rx = ^[ 	]*:global-test-result:[ 	]*
am__copy_in_global_log_rx = ^[ 	]*:copy-in-global-log:[ 	]*
am__list_recheck_tests = $(AWK) '{ \
  recheck = 1; \
  while ((rc = (getline line < ($$0 ".trs"))) != 0) \
    { \
      if (rc < 0) \
        { \
          if ((getline line2 < ($$0 ".log")) < 0) \
	    recheck = 0; \
          break; \
        } \
      else if (line ~ /$(am__recheck_rx)[nN][Oo]/) \
        { \
          recheck = 0; \
          break; \
        } \
      else if (line ~ /$(am__recheck_rx)[yY][eE][sS]/) \
        { \
          break; \
        } \
    }; \
  if (recheck) \
    print $$0; \
  close ($$0 ".trs"); \
  close ($$0 ".log"); \
}'
am__create_global_log = $(AWK) ' \
function fatal(msg) \
{ \
  print "fatal: making $@: " msg | "cat >&2"; \
  exit 1; \
} \
function rst_section(header) \
{ \
  print header; \
  len = length(header); \
  for (i = 1; i <= len; i = i + 1) \
    printf "="; \
  printf "\n\n"; \
} \
{ \
  copy_in_global_log = 1; \
  global_test_result = "RUN"; \
  while ((rc = (getline line < ($$0 ".trs"))) != 0) \
    { \
      if (rc < 0) \
         fatal("failed to read from " $$0 ".trs"); \
      if (line ~ /$(am__global_test_result_rx)/) \
        { \
          sub("$(am__global_test_result_rx)", "", line); \
          sub("[ 	]*$$", "", line); \
          global_test_result = line; \
        } \
      else if (line ~ /$(am__copy_in_global_log_rx)[nN][oO]/) \
        copy_in_global_log = 0; \
    }; \
  if (copy_in_global_log) \
    { \
      rst_section(global_test_result ": " $$0); \
      while ((rc = (getline line < ($$0 ".log"))) != 0) \
      { \
        if (rc < 0) \
          fatal("failed to read from " $$0 ".log"); \
        print line; \
      }; \
      printf "\n"; \
    }; \
  close ($$0 ".trs"); \
  close ($$0 ".log"); \
}'
am__rst_title = { sed 's/.*/   &   /;h;s/./=/g;p;x;s/ *$$//;p;g' && echo; }
am__sh_e_setup = case $$- in *e*) set +e;; esac
am__common_driver_flags = \
  --color-tests "$$am__color_tests" \
  --enable-hard-errors "$$am__enable_hard_errors" \
  --expect-failure "$$am__expect_failure"
am__check_pre = \
$(am__sh_e_setup);					\
$(am__vpath_adj_setup) $(am__vpath_adj)			\
$(am__tty_colors);					\
srcdir=$(srcdir); export srcdir;			\
case "$@" in						\
  */*) am__odir=`echo "./$@" | sed 's|/[^/]*$$||'`;;	\
    *) am__odir=.;; 					\
esac;							\
test "x$$am__odir" = x"." || test -d "$$am__odir" 	\
  || $(MKDIR_P) "$$am__odir" || exit $$?;		\
if test -f "./$$f"; then dir=./;			\
elif test -f "$$f"; then dir=;				\
else dir="$(srcdir)/"; fi;				\
tst=$$dir$$f; log='$@'; 				\
if test -n '$(DISABLE_HARD_ERRORS)'; then		\
  am__enable_hard_errors=no; 				\
else							\
  am__enable_hard_errors=yes; 				\
fi; 							\
case " $(XFAIL_TESTS) " in				\
  *[\ \	]$$f[\ \	]* | *[\ \	]$$dir$$f[\ \	]*) \
    am__expect_failure=yes;;				\
  *)							\
    am__expect_failure=no;;				\
esac; 							\
$(AM_TESTS_ENVIRONMENT) $(TESTS_ENVIRONMENT)
am__set_TESTS_bases = \
  bases='$(TEST_LOGS)'; \
  bases=`for i in $$bases; do echo $$i; done | sed 's/\.log$$//'`; \
  bases=`echo $$bases`
RECHECK_LOGS = $(TEST_LOGS)
AM_RECURSIVE_TARGETS = check recheck
TEST_SUITE_LOG = test-suite.log
TEST_EXTENSIONS =  .test
LOG_DRIVER = $(SHELL) $(top_srcdir)/test-driver
LOG_COMPILE = $(LOG_COMPILER) $(AM_LOG_FLAGS) $(LOG_FLAGS)
am__set_b = \
  case '$@' in \
    */*) \
      case '$*' in \
        */*) b='$*';; \
          *) b=`echo '$@' | sed 's/\.log$$//'`; \
       esac;; \
    *) \
      b='$*';; \
  esac
am__test_logs1 = $(TESTS:=.log)
am__test_logs2 = $(am__test_logs1:.log=.log)
TEST_LOGS = $(am__test_logs2:.test.log=.log)
TEST_LOG_DRIVER = $(SHELL) $(top_srcdir)/test-driver
TEST_LOG_COMPILE = $(TEST_LOG_COMPILER) $(AM_TEST_LOG_FLAGS) \
	$(TEST_LOG_FLAGS)
am__DIST_COMMON = $(srcdir)/Makefile.in $(top_srcdir)/depcomp \
	$(top_srcdir)/mkinstalldirs $(top_srcdir)/test-driver
DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)
ACLOCAL = ${SHELL} /home/hlechner/git/hl-test/amule/missing aclocal-1.15
ALCC_MANPAGES = 
ALC_MANPAGES = 
ALLOCA = 
AMTAR = $${TAR-tar}
AMULECMD_MANPAGES = 
AMULEWEB_DEFS = 
AMULE_DAEMON_MANPAGES = 
AMULE_GUI_MANPAGES = 
AR = ar
AUTOCONF = ${SHELL} /home/hlechner/git/hl-test/amule/missing autoconf
AUTOHEADER = ${SHELL} /home/hlechner/git/hl-test/amule/missing autoheader
AUTOMAKE = ${SHELL} /home/hlechner/git/hl-test/amule/missing automake-1.15
AWK = gawk
BFD_CPPFLAGS = -DHAVE_BFD
BFD_LIBS = -lbfd -liberty -ldl -lz
BOOST_CPPFLAGS = 
BOOST_LDPATH = 
BOOST_ROOT = 
BOOST_SYSTEM_LDFLAGS = 
BOOST_SYSTEM_LDPATH = 
BOOST_SYSTEM_LIBS = 
BUILD_CC = gcc
BUILD_CFLAGS = 
BUILD_CPPFLAGS = 
BUILD_EXEEXT = 
BUILD_INCLUDED_LIBINTL = no
BUILD_LDFLAGS = 
CAS_DEFS = 
CAS_MANPAGES = 
CATOBJEXT = .gmo
CC = gcc
CCDEPMODE = depmode=gcc3
CFLAGS = 
CPP = gcc -E
CPPFLAGS = 
CRYPTOPP_CPPFLAGS = -I/usr/include -D__WEAK_CRYPTO__
CRYPTOPP_LDFLAGS = -L/usr/lib
CRYPTOPP_LIBS = -lcryptopp
CXX = g++
CXXCPP = g++ -E
CXXDEPMODE = depmode=gcc3
CXXFLAGS = 
CYGPATH_W = echo
DATADIRNAME = share
DEBUGFLAG = 
DEFS = -DHAVE_CONFIG_H
DEPDIR = .deps
DISTCHECK_CONFIGURE_FLAGS = 
ECHO_C = 
ECHO_N = -n
ECHO_T = 
ED2K_MANPAGES = ed2k.1 ed2k.de.1 ed2k.es.1 ed2k.fr.1 ed2k.hu.1 ed2k.it.1 ed2k.ro.1 ed2k.ru.1 ed2k.tr.1 ed2k.zh_TW.1
EGREP = /usr/bin/grep -E
EXEEXT = 
GDLIB_CFLAGS = 
GDLIB_CONFIG_PATH = 
GDLIB_LDFLAGS = 
GDLIB_LIBS = 
GENCAT = gencat
GEOIP_CPPFLAGS = 
GEOIP_LDFLAGS = 
GEOIP_LIBS = 
GLIBC21 = yes
GMSGFMT = /usr/bin/msgfmt
GREP = /usr/bin/grep
INCINTL = 
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_STRIP_PROGRAM = $(install_sh) -c -s
INSTOBJEXT = .mo
INTLBISON = bison
INTLLIBS = 
INTLOBJS = 
INTL_LIBTOOL_SUFFIX_PREFIX = 
KDE4_CONFIG = 
KDE_APPLNK_PATH = 
KDE_HEADER_DIR = 
KDE_ICON_PATH = 
KDE_MIME_PATH = 
KDE_MODULE_PATH = 
KDE_SERVICE_PATH = 
LD = ld
LDFLAGS = 
LEX = flex
LEXLIB = -lfl
LEX_OUTPUT_ROOT = lex.yy
LIBICONV = 
LIBINTL = 
LIBOBJS = 
LIBPNG_CFLAGS = 
LIBPNG_CONFIG_PATH = 
LIBPNG_LDFLAGS = 
LIBPNG_LIBS = 
LIBS = 
LIBUPNP_CFLAGS = -pthread
LIBUPNP_CPPFLAGS = -I/usr/include/upnp -DENABLE_UPNP=1
LIBUPNP_LDFLAGS = 
LIBUPNP_LIBS = -pthread -lupnp -lthreadutil -lixml
LTLIBICONV = 
LTLIBINTL = 
LTLIBOBJS = 
MAINT = #
MAKEINFO = ${SHELL} /home/hlechner/git/hl-test/amule/missing makeinfo
MKDIR_P = /usr/bin/mkdir -p
MKINSTALLDIRS = ./mkinstalldirs
MONOLITHIC_MANPAGES = amule.1 amule.de.1 amule.es.1 amule.fr.1 amule.hu.1 amule.it.1 amule.ro.1 amule.ru.1 amule.tr.1 amule.zh_TW.1
MSGFMT = /usr/bin/msgfmt
MSGMERGE = /usr/bin/msgmerge
MULECFLAGS = -w  -g -ggdb -fno-inline -fmessage-length=0
MULECPPFLAGS =  -D__DEBUG__ -DUSE_WX_EXTENSIONS
MULECXXFLAGS = -w  -g -ggdb -fno-inline -fmessage-length=0
MULELDFLAGS =  -lpthread
MULERCFLAGS = -I/usr/lib/wx/include/gtk2-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__ 
OBJEXT = o
PACKAGE = amule
PACKAGE_BUGREPORT = admin@amule.org
PACKAGE_NAME = aMule
PACKAGE_STRING = aMule SVN
PACKAGE_TARNAME = amule
PACKAGE_URL = 
PACKAGE_VERSION = SVN
PATH_SEPARATOR = :
PKG_CONFIG = /usr/bin/pkg-config
POSUB = po
POW_LIB = 
QT_CORE_CXXFLAGS = 
QT_CORE_LIBS = 
QT_DBUS_LDFLAGS = 
QT_GUI_CXXFLAGS = 
QT_GUI_LIBS = 
QT_MOC = 
RANLIB = ranlib
RC = 
RCFLAGS = 
READLINE_LIBS = 
RESOLV_LIB = 
SED = /usr/bin/sed
SET_MAKE = 
SHELL = /bin/sh
STRIP = strip
SVNDATE = 
USE_INCLUDED_LIBINTL = no
USE_NLS = yes
VERSION = SVN
WEBSERVER_MANPAGES = 
WXBASE_CPPFLAGS = -I/usr/lib/wx/include/gtk2-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__ -DwxUSE_GUI=0
WXBASE_LIBS = -pthread   -lwx_baseu_net-3.0 -lwx_baseu-3.0 
WXCAS_MANPAGES = 
WX_CFLAGS = -I/usr/lib/wx/include/gtk2-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__ -pthread
WX_CFLAGS_ONLY = -pthread
WX_CONFIG_PATH = /usr/bin/wx-config
WX_CPPFLAGS = -I/usr/lib/wx/include/gtk2-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__
WX_CXXFLAGS = -I/usr/lib/wx/include/gtk2-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__ -pthread
WX_CXXFLAGS_ONLY = 
WX_DEBUG = 0
WX_LIBS = -pthread   -lwx_gtk2u_adv-3.0 -lwx_gtk2u_core-3.0 -lwx_baseu_net-3.0 -lwx_baseu-3.0 
WX_PORT = gtk2
WX_RESCOMP = 
WX_SHARED = 1
WX_UNICODE = 
WX_VERSION_FULL = 3.0.2
WX_VERSION_MAJOR = 3
WX_VERSION_MINOR = 0
XGETTEXT = /usr/bin/xgettext
YACC = bison -y
YFLAGS = 
ZLIB_CPPFLAGS = 
ZLIB_LDFLAGS = 
ZLIB_LIBS = -lz
abs_builddir = /home/hlechner/git/hl-test/amule/unittests/tests
abs_srcdir = /home/hlechner/git/hl-test/amule/unittests/tests
abs_top_builddir = /home/hlechner/git/hl-test/amule
abs_top_srcdir = /home/hlechner/git/hl-test/amule
ac_ct_BUILD_CC = 
ac_ct_CC = gcc
ac_ct_CXX = g++
am__include = include
am__leading_dot = .
am__quote = 
am__tar = $${TAR-tar} chof - "$$tardir"
am__untar = $${TAR-tar} xf -
bindir = ${exec_prefix}/bin
build = x86_64-unknown-linux-gnu
build_alias = 
build_cpu = x86_64
build_os = linux-gnu
build_vendor = unknown
builddir = .
datadir = ${datarootdir}
datarootdir = ${prefix}/share
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
dvidir = ${docdir}
exec_prefix = ${prefix}
host = x86_64-unknown-linux-gnu
host_alias = 
host_cpu = x86_64
host_os = linux-gnu
host_vendor = unknown
htmldir = ${docdir}
includedir = ${prefix}/include
infodir = ${datarootdir}/info
install_sh = ${SHELL} /home/hlechner/git/hl-test/amule/install-sh
libdir = ${exec_prefix}/lib
libexecdir = ${exec_prefix}/libexec
localedir = ${datarootdir}/locale
localstatedir = ${prefix}/var
mandir = ${datarootdir}/man
mkdir_p = $(MKDIR_P)
oldincludedir = /usr/include
pdfdir = ${docdir}
prefix = /usr/local
program_transform_name = s,x,x,
psdir = ${docdir}
sbindir = ${exec_prefix}/sbin
sharedstatedir = ${prefix}/com
srcdir = .
sysconfdir = ${prefix}/etc
target_alias = 
top_build_prefix = ../../
top_builddir = ../..
top_srcdir = ../..
EXTRA_DIST = TextFileTest_dos.txt TextFileTest_unix.txt
MUCPPFLAGS = -DMULEUNIT
AM_CPPFLAGS = $(MULECPPFLAGS) -I$(srcdir) -I$(srcdir)/.. -I$(top_srcdir)/src -I$(top_srcdir)/src/libs -I$(top_srcdir)/src/include $(MUCPPFLAGS) $(WXBASE_CPPFLAGS)
AM_CXXFLAGS = $(MULECXXFLAGS) $(WX_CFLAGS_ONLY) $(WX_CXXFLAGS_ONLY)
AM_LDFLAGS = $(MULELDFLAGS)
LDADD = $(WXBASE_LIBS) ../muleunit/libmuleunit.a
MAINTAINERCLEANFILES = Makefile.in
CUInt128Test_SOURCES = CUInt128Test.cpp $(top_srcdir)/src/kademlia/utils/UInt128.cpp $(top_srcdir)/src/libs/common/Format.cpp $(top_srcdir)/src/libs/common/strerror_r.c
RangeMapTest_SOURCES = RangeMapTest.cpp
FormatTest_SOURCES = FormatTest.cpp $(top_srcdir)/src/libs/common/Format.cpp $(top_srcdir)/src/libs/common/strerror_r.c
StringFunctionsTest_SOURCES = StringFunctionsTest.cpp $(top_srcdir)/src/libs/common/StringFunctions.cpp $(top_srcdir)/src/libs/common/Path.cpp
NetworkFunctionsTest_SOURCES = NetworkFunctionsTest.cpp $(top_srcdir)/src/NetworkFunctions.cpp $(top_srcdir)/src/LibSocket.cpp $(top_srcdir)/src/libs/common/Format.cpp $(top_srcdir)/src/libs/common/strerror_r.c
NetworkFunctionsTest_CPPFLAGS = $(AM_CPPFLAGS) -DEC_REMOTE # Needed to avoid compiling the http-thread
FileDataIOTest_SOURCES = FileDataIOTest.cpp $(top_srcdir)/src/SafeFile.cpp $(top_srcdir)/src/CFile.cpp $(top_srcdir)/src/MemFile.cpp $(top_srcdir)/src/kademlia/utils/UInt128.cpp $(top_srcdir)/src/libs/common/StringFunctions.cpp $(top_srcdir)/src/Tag.cpp $(top_srcdir)/src/libs/common/Path.cpp $(top_srcdir)/src/libs/common/Format.cpp $(top_srcdir)/src/libs/common/strerror_r.c
PathTest_SOURCES = PathTest.cpp $(top_srcdir)/src/libs/common/Path.cpp $(top_srcdir)/src/libs/common/StringFunctions.cpp
TextFileTest_SOURCES = TextFileTest.cpp $(top_srcdir)/src/libs/common/Path.cpp $(top_srcdir)/src/libs/common/StringFunctions.cpp $(top_srcdir)/src/libs/common/TextFile.cpp
TextFileTest_CPPFLAGS = $(AM_CPPFLAGS) -DSRCDIR="$(srcdir)"
CTagTest_SOURCES = CTagTest.cpp  $(top_srcdir)/src/SafeFile.cpp  $(top_srcdir)/src/MemFile.cpp $(top_srcdir)/src/Tag.cpp $(top_srcdir)/src/libs/common/Format.cpp $(top_srcdir)/src/libs/common/strerror_r.c
all: all-am
.SUFFIXES:
.SUFFIXES: .c .cpp .log .o .obj .test .test$(EXEEXT) .trs
$(srcdir)/Makefile.in: # $(srcdir)/Makefile.am  $(am__configure_deps)
	@for dep in $?; do \
	  case '$(am__configure_deps)' in \
	    *$$dep*) \
	      ( cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh ) \
	        && { if test -f $@; then exit 0; else break; fi; }; \
	      exit 1;; \
	  esac; \
	done; \
	echo ' cd $(top_srcdir) && $(AUTOMAKE) --foreign unittests/tests/Makefile'>/dev/null; \
	$(am__cd) $(top_srcdir) && \
	  $(AUTOMAKE) --foreign unittests/tests/Makefile
Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	@case '$?' in \
	  *config.status*) \
	    cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh;; \
	  *) \
	    echo ' cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe)'>/dev/null; \
	    cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe) denoiser;; \
	esac;
$(top_builddir)/config.status: $(top_srcdir)/configure $(CONFIG_STATUS_DEPENDENCIES)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(top_srcdir)/configure: # $(am__configure_deps)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(ACLOCAL_M4): # $(am__aclocal_m4_deps)
	cd $(top_builddir) && $(MAKE) $(AM_MAKEFLAGS) am--refresh
$(am__aclocal_m4_deps):
clean-checkPROGRAMS:
	-test -z "$(check_PROGRAMS)" || rm -f $(check_PROGRAMS)
CTagTest$(EXEEXT): $(CTagTest_OBJECTS) $(CTagTest_DEPENDENCIES) $(EXTRA_CTagTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f CTagTest$(EXEEXT)
	$(CXXLINK) $(CTagTest_OBJECTS) $(CTagTest_LDADD) $(LIBS)
CUInt128Test$(EXEEXT): $(CUInt128Test_OBJECTS) $(CUInt128Test_DEPENDENCIES) $(EXTRA_CUInt128Test_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f CUInt128Test$(EXEEXT)
	$(CXXLINK) $(CUInt128Test_OBJECTS) $(CUInt128Test_LDADD) $(LIBS)
FileDataIOTest$(EXEEXT): $(FileDataIOTest_OBJECTS) $(FileDataIOTest_DEPENDENCIES) $(EXTRA_FileDataIOTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f FileDataIOTest$(EXEEXT)
	$(CXXLINK) $(FileDataIOTest_OBJECTS) $(FileDataIOTest_LDADD) $(LIBS)
FormatTest$(EXEEXT): $(FormatTest_OBJECTS) $(FormatTest_DEPENDENCIES) $(EXTRA_FormatTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f FormatTest$(EXEEXT)
	$(CXXLINK) $(FormatTest_OBJECTS) $(FormatTest_LDADD) $(LIBS)
NetworkFunctionsTest$(EXEEXT): $(NetworkFunctionsTest_OBJECTS) $(NetworkFunctionsTest_DEPENDENCIES) $(EXTRA_NetworkFunctionsTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f NetworkFunctionsTest$(EXEEXT)
	$(CXXLINK) $(NetworkFunctionsTest_OBJECTS) $(NetworkFunctionsTest_LDADD) $(LIBS)
PathTest$(EXEEXT): $(PathTest_OBJECTS) $(PathTest_DEPENDENCIES) $(EXTRA_PathTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f PathTest$(EXEEXT)
	$(CXXLINK) $(PathTest_OBJECTS) $(PathTest_LDADD) $(LIBS)
RangeMapTest$(EXEEXT): $(RangeMapTest_OBJECTS) $(RangeMapTest_DEPENDENCIES) $(EXTRA_RangeMapTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f RangeMapTest$(EXEEXT)
	$(CXXLINK) $(RangeMapTest_OBJECTS) $(RangeMapTest_LDADD) $(LIBS)
StringFunctionsTest$(EXEEXT): $(StringFunctionsTest_OBJECTS) $(StringFunctionsTest_DEPENDENCIES) $(EXTRA_StringFunctionsTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f StringFunctionsTest$(EXEEXT)
	$(CXXLINK) $(StringFunctionsTest_OBJECTS) $(StringFunctionsTest_LDADD) $(LIBS)
TextFileTest$(EXEEXT): $(TextFileTest_OBJECTS) $(TextFileTest_DEPENDENCIES) $(EXTRA_TextFileTest_DEPENDENCIES) 
	echo "Linking $(@F)"
	@rm -f TextFileTest$(EXEEXT)
	$(CXXLINK) $(TextFileTest_OBJECTS) $(TextFileTest_LDADD) $(LIBS)
mostlyclean-compile:
	-rm -f *.$(OBJEXT)
distclean-compile:
	-rm -f *.tab.c
include ./$(DEPDIR)/CFile.Po
include ./$(DEPDIR)/CTagTest.Po
include ./$(DEPDIR)/CUInt128Test.Po
include ./$(DEPDIR)/FileDataIOTest.Po
include ./$(DEPDIR)/Format.Po
include ./$(DEPDIR)/FormatTest.Po
include ./$(DEPDIR)/MemFile.Po
include ./$(DEPDIR)/NetworkFunctionsTest-Format.Po
include ./$(DEPDIR)/NetworkFunctionsTest-LibSocket.Po
include ./$(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Po
include ./$(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Po
include ./$(DEPDIR)/NetworkFunctionsTest-strerror_r.Po
include ./$(DEPDIR)/Path.Po
include ./$(DEPDIR)/PathTest.Po
include ./$(DEPDIR)/RangeMapTest.Po
include ./$(DEPDIR)/SafeFile.Po
include ./$(DEPDIR)/StringFunctions.Po
include ./$(DEPDIR)/StringFunctionsTest.Po
include ./$(DEPDIR)/Tag.Po
include ./$(DEPDIR)/TextFileTest-Path.Po
include ./$(DEPDIR)/TextFileTest-StringFunctions.Po
include ./$(DEPDIR)/TextFileTest-TextFile.Po
include ./$(DEPDIR)/TextFileTest-TextFileTest.Po
include ./$(DEPDIR)/UInt128.Po
include ./$(DEPDIR)/strerror_r.Po
.c.o:
	echo "Compiling $(<F)"
	$(COMPILE) -MT $@ -MD -MP -MF $(DEPDIR)/$*.Tpo -c -o $@ $<
	$(am__mv) $(DEPDIR)/$*.Tpo $(DEPDIR)/$*.Po
.c.obj:
	echo "Compiling $(<F)"
	$(COMPILE) -MT $@ -MD -MP -MF $(DEPDIR)/$*.Tpo -c -o $@ `$(CYGPATH_W) '$<'`
	$(am__mv) $(DEPDIR)/$*.Tpo $(DEPDIR)/$*.Po
strerror_r.o: $(top_srcdir)/src/libs/common/strerror_r.c
	echo "Compiling $(<F)"
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -MT strerror_r.o -MD -MP -MF $(DEPDIR)/strerror_r.Tpo -c -o strerror_r.o `test -f '$(top_srcdir)/src/libs/common/strerror_r.c' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/strerror_r.c
	$(am__mv) $(DEPDIR)/strerror_r.Tpo $(DEPDIR)/strerror_r.Po
strerror_r.obj: $(top_srcdir)/src/libs/common/strerror_r.c
	echo "Compiling $(<F)"
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -MT strerror_r.obj -MD -MP -MF $(DEPDIR)/strerror_r.Tpo -c -o strerror_r.obj `if test -f '$(top_srcdir)/src/libs/common/strerror_r.c'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/strerror_r.c'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/strerror_r.c'; fi`
	$(am__mv) $(DEPDIR)/strerror_r.Tpo $(DEPDIR)/strerror_r.Po
NetworkFunctionsTest-strerror_r.o: $(top_srcdir)/src/libs/common/strerror_r.c
	echo "Compiling $(<F)"
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -MT NetworkFunctionsTest-strerror_r.o -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-strerror_r.Tpo -c -o NetworkFunctionsTest-strerror_r.o `test -f '$(top_srcdir)/src/libs/common/strerror_r.c' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/strerror_r.c
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-strerror_r.Tpo $(DEPDIR)/NetworkFunctionsTest-strerror_r.Po
NetworkFunctionsTest-strerror_r.obj: $(top_srcdir)/src/libs/common/strerror_r.c
	echo "Compiling $(<F)"
	$(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) -MT NetworkFunctionsTest-strerror_r.obj -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-strerror_r.Tpo -c -o NetworkFunctionsTest-strerror_r.obj `if test -f '$(top_srcdir)/src/libs/common/strerror_r.c'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/strerror_r.c'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/strerror_r.c'; fi`
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-strerror_r.Tpo $(DEPDIR)/NetworkFunctionsTest-strerror_r.Po
.cpp.o:
	echo "Compiling $(<F)"
	$(CXXCOMPILE) -MT $@ -MD -MP -MF $(DEPDIR)/$*.Tpo -c -o $@ $<
	$(am__mv) $(DEPDIR)/$*.Tpo $(DEPDIR)/$*.Po
.cpp.obj:
	echo "Compiling $(<F)"
	$(CXXCOMPILE) -MT $@ -MD -MP -MF $(DEPDIR)/$*.Tpo -c -o $@ `$(CYGPATH_W) '$<'`
	$(am__mv) $(DEPDIR)/$*.Tpo $(DEPDIR)/$*.Po
SafeFile.o: $(top_srcdir)/src/SafeFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT SafeFile.o -MD -MP -MF $(DEPDIR)/SafeFile.Tpo -c -o SafeFile.o `test -f '$(top_srcdir)/src/SafeFile.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/SafeFile.cpp
	$(am__mv) $(DEPDIR)/SafeFile.Tpo $(DEPDIR)/SafeFile.Po
SafeFile.obj: $(top_srcdir)/src/SafeFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT SafeFile.obj -MD -MP -MF $(DEPDIR)/SafeFile.Tpo -c -o SafeFile.obj `if test -f '$(top_srcdir)/src/SafeFile.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/SafeFile.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/SafeFile.cpp'; fi`
	$(am__mv) $(DEPDIR)/SafeFile.Tpo $(DEPDIR)/SafeFile.Po
MemFile.o: $(top_srcdir)/src/MemFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT MemFile.o -MD -MP -MF $(DEPDIR)/MemFile.Tpo -c -o MemFile.o `test -f '$(top_srcdir)/src/MemFile.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/MemFile.cpp
	$(am__mv) $(DEPDIR)/MemFile.Tpo $(DEPDIR)/MemFile.Po
MemFile.obj: $(top_srcdir)/src/MemFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT MemFile.obj -MD -MP -MF $(DEPDIR)/MemFile.Tpo -c -o MemFile.obj `if test -f '$(top_srcdir)/src/MemFile.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/MemFile.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/MemFile.cpp'; fi`
	$(am__mv) $(DEPDIR)/MemFile.Tpo $(DEPDIR)/MemFile.Po
Tag.o: $(top_srcdir)/src/Tag.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT Tag.o -MD -MP -MF $(DEPDIR)/Tag.Tpo -c -o Tag.o `test -f '$(top_srcdir)/src/Tag.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/Tag.cpp
	$(am__mv) $(DEPDIR)/Tag.Tpo $(DEPDIR)/Tag.Po
Tag.obj: $(top_srcdir)/src/Tag.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT Tag.obj -MD -MP -MF $(DEPDIR)/Tag.Tpo -c -o Tag.obj `if test -f '$(top_srcdir)/src/Tag.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/Tag.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/Tag.cpp'; fi`
	$(am__mv) $(DEPDIR)/Tag.Tpo $(DEPDIR)/Tag.Po
Format.o: $(top_srcdir)/src/libs/common/Format.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT Format.o -MD -MP -MF $(DEPDIR)/Format.Tpo -c -o Format.o `test -f '$(top_srcdir)/src/libs/common/Format.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/Format.cpp
	$(am__mv) $(DEPDIR)/Format.Tpo $(DEPDIR)/Format.Po
Format.obj: $(top_srcdir)/src/libs/common/Format.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT Format.obj -MD -MP -MF $(DEPDIR)/Format.Tpo -c -o Format.obj `if test -f '$(top_srcdir)/src/libs/common/Format.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/Format.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/Format.cpp'; fi`
	$(am__mv) $(DEPDIR)/Format.Tpo $(DEPDIR)/Format.Po
UInt128.o: $(top_srcdir)/src/kademlia/utils/UInt128.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT UInt128.o -MD -MP -MF $(DEPDIR)/UInt128.Tpo -c -o UInt128.o `test -f '$(top_srcdir)/src/kademlia/utils/UInt128.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/kademlia/utils/UInt128.cpp
	$(am__mv) $(DEPDIR)/UInt128.Tpo $(DEPDIR)/UInt128.Po
UInt128.obj: $(top_srcdir)/src/kademlia/utils/UInt128.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT UInt128.obj -MD -MP -MF $(DEPDIR)/UInt128.Tpo -c -o UInt128.obj `if test -f '$(top_srcdir)/src/kademlia/utils/UInt128.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/kademlia/utils/UInt128.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/kademlia/utils/UInt128.cpp'; fi`
	$(am__mv) $(DEPDIR)/UInt128.Tpo $(DEPDIR)/UInt128.Po
CFile.o: $(top_srcdir)/src/CFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT CFile.o -MD -MP -MF $(DEPDIR)/CFile.Tpo -c -o CFile.o `test -f '$(top_srcdir)/src/CFile.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/CFile.cpp
	$(am__mv) $(DEPDIR)/CFile.Tpo $(DEPDIR)/CFile.Po
CFile.obj: $(top_srcdir)/src/CFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT CFile.obj -MD -MP -MF $(DEPDIR)/CFile.Tpo -c -o CFile.obj `if test -f '$(top_srcdir)/src/CFile.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/CFile.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/CFile.cpp'; fi`
	$(am__mv) $(DEPDIR)/CFile.Tpo $(DEPDIR)/CFile.Po
StringFunctions.o: $(top_srcdir)/src/libs/common/StringFunctions.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT StringFunctions.o -MD -MP -MF $(DEPDIR)/StringFunctions.Tpo -c -o StringFunctions.o `test -f '$(top_srcdir)/src/libs/common/StringFunctions.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/StringFunctions.cpp
	$(am__mv) $(DEPDIR)/StringFunctions.Tpo $(DEPDIR)/StringFunctions.Po
StringFunctions.obj: $(top_srcdir)/src/libs/common/StringFunctions.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT StringFunctions.obj -MD -MP -MF $(DEPDIR)/StringFunctions.Tpo -c -o StringFunctions.obj `if test -f '$(top_srcdir)/src/libs/common/StringFunctions.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/StringFunctions.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/StringFunctions.cpp'; fi`
	$(am__mv) $(DEPDIR)/StringFunctions.Tpo $(DEPDIR)/StringFunctions.Po
Path.o: $(top_srcdir)/src/libs/common/Path.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT Path.o -MD -MP -MF $(DEPDIR)/Path.Tpo -c -o Path.o `test -f '$(top_srcdir)/src/libs/common/Path.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/Path.cpp
	$(am__mv) $(DEPDIR)/Path.Tpo $(DEPDIR)/Path.Po
Path.obj: $(top_srcdir)/src/libs/common/Path.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT Path.obj -MD -MP -MF $(DEPDIR)/Path.Tpo -c -o Path.obj `if test -f '$(top_srcdir)/src/libs/common/Path.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/Path.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/Path.cpp'; fi`
	$(am__mv) $(DEPDIR)/Path.Tpo $(DEPDIR)/Path.Po
NetworkFunctionsTest-NetworkFunctionsTest.o: NetworkFunctionsTest.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-NetworkFunctionsTest.o -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Tpo -c -o NetworkFunctionsTest-NetworkFunctionsTest.o `test -f 'NetworkFunctionsTest.cpp' || echo '$(srcdir)/'`NetworkFunctionsTest.cpp
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Tpo $(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Po
NetworkFunctionsTest-NetworkFunctionsTest.obj: NetworkFunctionsTest.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-NetworkFunctionsTest.obj -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Tpo -c -o NetworkFunctionsTest-NetworkFunctionsTest.obj `if test -f 'NetworkFunctionsTest.cpp'; then $(CYGPATH_W) 'NetworkFunctionsTest.cpp'; else $(CYGPATH_W) '$(srcdir)/NetworkFunctionsTest.cpp'; fi`
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Tpo $(DEPDIR)/NetworkFunctionsTest-NetworkFunctionsTest.Po
NetworkFunctionsTest-NetworkFunctions.o: $(top_srcdir)/src/NetworkFunctions.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-NetworkFunctions.o -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Tpo -c -o NetworkFunctionsTest-NetworkFunctions.o `test -f '$(top_srcdir)/src/NetworkFunctions.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/NetworkFunctions.cpp
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Tpo $(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Po
NetworkFunctionsTest-NetworkFunctions.obj: $(top_srcdir)/src/NetworkFunctions.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-NetworkFunctions.obj -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Tpo -c -o NetworkFunctionsTest-NetworkFunctions.obj `if test -f '$(top_srcdir)/src/NetworkFunctions.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/NetworkFunctions.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/NetworkFunctions.cpp'; fi`
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Tpo $(DEPDIR)/NetworkFunctionsTest-NetworkFunctions.Po
NetworkFunctionsTest-LibSocket.o: $(top_srcdir)/src/LibSocket.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-LibSocket.o -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-LibSocket.Tpo -c -o NetworkFunctionsTest-LibSocket.o `test -f '$(top_srcdir)/src/LibSocket.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/LibSocket.cpp
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-LibSocket.Tpo $(DEPDIR)/NetworkFunctionsTest-LibSocket.Po
NetworkFunctionsTest-LibSocket.obj: $(top_srcdir)/src/LibSocket.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-LibSocket.obj -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-LibSocket.Tpo -c -o NetworkFunctionsTest-LibSocket.obj `if test -f '$(top_srcdir)/src/LibSocket.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/LibSocket.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/LibSocket.cpp'; fi`
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-LibSocket.Tpo $(DEPDIR)/NetworkFunctionsTest-LibSocket.Po
NetworkFunctionsTest-Format.o: $(top_srcdir)/src/libs/common/Format.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-Format.o -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-Format.Tpo -c -o NetworkFunctionsTest-Format.o `test -f '$(top_srcdir)/src/libs/common/Format.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/Format.cpp
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-Format.Tpo $(DEPDIR)/NetworkFunctionsTest-Format.Po
NetworkFunctionsTest-Format.obj: $(top_srcdir)/src/libs/common/Format.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(NetworkFunctionsTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT NetworkFunctionsTest-Format.obj -MD -MP -MF $(DEPDIR)/NetworkFunctionsTest-Format.Tpo -c -o NetworkFunctionsTest-Format.obj `if test -f '$(top_srcdir)/src/libs/common/Format.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/Format.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/Format.cpp'; fi`
	$(am__mv) $(DEPDIR)/NetworkFunctionsTest-Format.Tpo $(DEPDIR)/NetworkFunctionsTest-Format.Po
TextFileTest-TextFileTest.o: TextFileTest.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-TextFileTest.o -MD -MP -MF $(DEPDIR)/TextFileTest-TextFileTest.Tpo -c -o TextFileTest-TextFileTest.o `test -f 'TextFileTest.cpp' || echo '$(srcdir)/'`TextFileTest.cpp
	$(am__mv) $(DEPDIR)/TextFileTest-TextFileTest.Tpo $(DEPDIR)/TextFileTest-TextFileTest.Po
TextFileTest-TextFileTest.obj: TextFileTest.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-TextFileTest.obj -MD -MP -MF $(DEPDIR)/TextFileTest-TextFileTest.Tpo -c -o TextFileTest-TextFileTest.obj `if test -f 'TextFileTest.cpp'; then $(CYGPATH_W) 'TextFileTest.cpp'; else $(CYGPATH_W) '$(srcdir)/TextFileTest.cpp'; fi`
	$(am__mv) $(DEPDIR)/TextFileTest-TextFileTest.Tpo $(DEPDIR)/TextFileTest-TextFileTest.Po
TextFileTest-Path.o: $(top_srcdir)/src/libs/common/Path.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-Path.o -MD -MP -MF $(DEPDIR)/TextFileTest-Path.Tpo -c -o TextFileTest-Path.o `test -f '$(top_srcdir)/src/libs/common/Path.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/Path.cpp
	$(am__mv) $(DEPDIR)/TextFileTest-Path.Tpo $(DEPDIR)/TextFileTest-Path.Po
TextFileTest-Path.obj: $(top_srcdir)/src/libs/common/Path.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-Path.obj -MD -MP -MF $(DEPDIR)/TextFileTest-Path.Tpo -c -o TextFileTest-Path.obj `if test -f '$(top_srcdir)/src/libs/common/Path.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/Path.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/Path.cpp'; fi`
	$(am__mv) $(DEPDIR)/TextFileTest-Path.Tpo $(DEPDIR)/TextFileTest-Path.Po
TextFileTest-StringFunctions.o: $(top_srcdir)/src/libs/common/StringFunctions.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-StringFunctions.o -MD -MP -MF $(DEPDIR)/TextFileTest-StringFunctions.Tpo -c -o TextFileTest-StringFunctions.o `test -f '$(top_srcdir)/src/libs/common/StringFunctions.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/StringFunctions.cpp
	$(am__mv) $(DEPDIR)/TextFileTest-StringFunctions.Tpo $(DEPDIR)/TextFileTest-StringFunctions.Po
TextFileTest-StringFunctions.obj: $(top_srcdir)/src/libs/common/StringFunctions.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-StringFunctions.obj -MD -MP -MF $(DEPDIR)/TextFileTest-StringFunctions.Tpo -c -o TextFileTest-StringFunctions.obj `if test -f '$(top_srcdir)/src/libs/common/StringFunctions.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/StringFunctions.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/StringFunctions.cpp'; fi`
	$(am__mv) $(DEPDIR)/TextFileTest-StringFunctions.Tpo $(DEPDIR)/TextFileTest-StringFunctions.Po
TextFileTest-TextFile.o: $(top_srcdir)/src/libs/common/TextFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-TextFile.o -MD -MP -MF $(DEPDIR)/TextFileTest-TextFile.Tpo -c -o TextFileTest-TextFile.o `test -f '$(top_srcdir)/src/libs/common/TextFile.cpp' || echo '$(srcdir)/'`$(top_srcdir)/src/libs/common/TextFile.cpp
	$(am__mv) $(DEPDIR)/TextFileTest-TextFile.Tpo $(DEPDIR)/TextFileTest-TextFile.Po
TextFileTest-TextFile.obj: $(top_srcdir)/src/libs/common/TextFile.cpp
	echo "Compiling $(<F)"
	$(CXX) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(TextFileTest_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) -MT TextFileTest-TextFile.obj -MD -MP -MF $(DEPDIR)/TextFileTest-TextFile.Tpo -c -o TextFileTest-TextFile.obj `if test -f '$(top_srcdir)/src/libs/common/TextFile.cpp'; then $(CYGPATH_W) '$(top_srcdir)/src/libs/common/TextFile.cpp'; else $(CYGPATH_W) '$(srcdir)/$(top_srcdir)/src/libs/common/TextFile.cpp'; fi`
	$(am__mv) $(DEPDIR)/TextFileTest-TextFile.Tpo $(DEPDIR)/TextFileTest-TextFile.Po
ID: $(am__tagged_files)
	$(am__define_uniq_tagged_files); mkid -fID $$unique
tags: tags-am
TAGS: tags
tags-am: $(TAGS_DEPENDENCIES) $(am__tagged_files)
	set x; \
	here=`pwd`; \
	$(am__define_uniq_tagged_files); \
	shift; \
	if test -z "$(ETAGS_ARGS)$$*$$unique"; then :; else \
	  test -n "$$unique" || unique=$$empty_fix; \
	  if test $$# -gt 0; then \
	    $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	      "$$@" $$unique; \
	  else \
	    $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	      $$unique; \
	  fi; \
	fi
ctags: ctags-am
CTAGS: ctags
ctags-am: $(TAGS_DEPENDENCIES) $(am__tagged_files)
	$(am__define_uniq_tagged_files); \
	test -z "$(CTAGS_ARGS)$$unique" \
	  || $(CTAGS) $(CTAGSFLAGS) $(AM_CTAGSFLAGS) $(CTAGS_ARGS) \
	     $$unique
GTAGS:
	here=`$(am__cd) $(top_builddir) && pwd` \
	  && $(am__cd) $(top_srcdir) \
	  && gtags -i $(GTAGS_ARGS) "$$here"
cscopelist: cscopelist-am
cscopelist-am: $(am__tagged_files)
	list='$(am__tagged_files)'; \
	case "$(srcdir)" in \
	  [\\/]* | ?:[\\/]*) sdir="$(srcdir)" ;; \
	  *) sdir=$(subdir)/$(srcdir) ;; \
	esac; \
	for i in $$list; do \
	  if test -f "$$i"; then \
	    echo "$(subdir)/$$i">/dev/null; \
	  else \
	    echo "$$sdir/$$i">/dev/null; \
	  fi; \
	done >> $(top_builddir)/cscope.files
distclean-tags:
	-rm -f TAGS ID GTAGS GRTAGS GSYMS GPATH tags
.log.trs:
	rm -f $< $@
	$(MAKE) $(AM_MAKEFLAGS) $<
am--fnord $(TEST_LOGS) $(TEST_LOGS:.log=.trs): $(am__force_recheck)
am--force-recheck:
	@:
$(TEST_SUITE_LOG): $(TEST_LOGS)
	@$(am__set_TESTS_bases); \
	am__f_ok () { test -f "$$1" && test -r "$$1"; }; \
	redo_bases=`for i in $$bases; do \
	              am__f_ok $$i.trs && am__f_ok $$i.log || echo $$i; \
	            done`; \
	if test -n "$$redo_bases"; then \
	  redo_logs=`for i in $$redo_bases; do echo $$i.log; done`; \
	  redo_results=`for i in $$redo_bases; do echo $$i.trs; done`; \
	  if $(am__make_dryrun); then :; else \
	    rm -f $$redo_logs && rm -f $$redo_results || exit 1; \
	  fi; \
	fi; \
	if test -n "$$am__remaking_logs"; then \
	  echo "fatal: making $(TEST_SUITE_LOG): possible infinite" \>/dev/null
	       "recursion detected" >&2; \
	elif test -n "$$redo_logs"; then \
	  am__remaking_logs=yes $(MAKE) $(AM_MAKEFLAGS) $$redo_logs; \
	fi; \
	if $(am__make_dryrun); then :; else \
	  st=0;  \
	  errmsg="fatal: making $(TEST_SUITE_LOG): failed to create"; \
	  for i in $$redo_bases; do \
	    test -f $$i.trs && test -r $$i.trs \
	      || { echo "$$errmsg $$i.trs" >&2; st=1; }; \
	    test -f $$i.log && test -r $$i.log \
	      || { echo "$$errmsg $$i.log" >&2; st=1; }; \
	  done; \
	  test $$st -eq 0 || exit 1; \
	fi
	@$(am__sh_e_setup); $(am__tty_colors); $(am__set_TESTS_bases); \
	ws='[ 	]'; \
	results=`for b in $$bases; do echo $$b.trs; done`; \
	test -n "$$results" || results=/dev/null; \
	all=`  grep "^$$ws*:test-result:"           $$results | wc -l`; \
	pass=` grep "^$$ws*:test-result:$$ws*PASS"  $$results | wc -l`; \
	fail=` grep "^$$ws*:test-result:$$ws*FAIL"  $$results | wc -l`; \
	skip=` grep "^$$ws*:test-result:$$ws*SKIP"  $$results | wc -l`; \
	xfail=`grep "^$$ws*:test-result:$$ws*XFAIL" $$results | wc -l`; \
	xpass=`grep "^$$ws*:test-result:$$ws*XPASS" $$results | wc -l`; \
	error=`grep "^$$ws*:test-result:$$ws*ERROR" $$results | wc -l`; \
	if test `expr $$fail + $$xpass + $$error` -eq 0; then \
	  success=true; \
	else \
	  success=false; \
	fi; \
	br='==================='; br=$$br$$br$$br$$br; \
	result_count () \
	{ \
	    if test x"$$1" = x"--maybe-color"; then \
	      maybe_colorize=yes; \
	    elif test x"$$1" = x"--no-color"; then \
	      maybe_colorize=no; \
	    else \
	      echo "$@: invalid 'result_count' usage" >&2; exit 4; \
	    fi; \
	    shift; \
	    desc=$$1 count=$$2; \
	    if test $$maybe_colorize = yes && test $$count -gt 0; then \
	      color_start=$$3 color_end=$$std; \
	    else \
	      color_start= color_end=; \
	    fi; \
	    echo "$${color_start}# $$desc $$count$${color_end}">/dev/null; \
	}; \
	create_testsuite_report () \
	{ \
	  result_count $$1 "TOTAL:" $$all   "$$brg"; \
	  result_count $$1 "PASS: " $$pass  "$$grn"; \
	  result_count $$1 "SKIP: " $$skip  "$$blu"; \
	  result_count $$1 "XFAIL:" $$xfail "$$lgn"; \
	  result_count $$1 "FAIL: " $$fail  "$$red"; \
	  result_count $$1 "XPASS:" $$xpass "$$red"; \
	  result_count $$1 "ERROR:" $$error "$$mgn"; \
	}; \
	{								\
	  echo "$(PACKAGE_STRING): $(subdir)/$(TEST_SUITE_LOG)" |	\>/dev/null
	    $(am__rst_title);						\
	  create_testsuite_report --no-color;				\
	  echo;								\
	  echo ".. contents:: :depth: 2">/dev/null;				\
	  echo;								\
	  for b in $$bases; do echo $$b; done				\
	    | $(am__create_global_log);					\
	} >$(TEST_SUITE_LOG).tmp || exit 1;				\
	mv $(TEST_SUITE_LOG).tmp $(TEST_SUITE_LOG);			\
	if $$success; then						\
	  col="$$grn";							\
	 else								\
	  col="$$red";							\
	  test x"$$VERBOSE" = x || cat $(TEST_SUITE_LOG);		\
	fi;								\
	echo "$${col}$$br$${std}">/dev/null; 					\
	echo "$${col}Testsuite summary for $(PACKAGE_STRING)$${std}">/dev/null;	\
	echo "$${col}$$br$${std}">/dev/null; 					\
	create_testsuite_report --maybe-color;				\
	echo "$$col$$br$$std">/dev/null;						\
	if $$success; then :; else					\
	  echo "$${col}See $(subdir)/$(TEST_SUITE_LOG)$${std}">/dev/null;		\
	  if test -n "$(PACKAGE_BUGREPORT)"; then			\
	    echo "$${col}Please report to $(PACKAGE_BUGREPORT)$${std}">/dev/null;	\
	  fi;								\
	  echo "$$col$$br$$std">/dev/null;					\
	fi;								\
	$$success || exit 1
check-TESTS:
	@list='$(RECHECK_LOGS)';           test -z "$$list" || rm -f $$list
	@list='$(RECHECK_LOGS:.log=.trs)'; test -z "$$list" || rm -f $$list
	@test -z "$(TEST_SUITE_LOG)" || rm -f $(TEST_SUITE_LOG)
	@set +e; $(am__set_TESTS_bases); \
	log_list=`for i in $$bases; do echo $$i.log; done`; \
	trs_list=`for i in $$bases; do echo $$i.trs; done`; \
	log_list=`echo $$log_list`; trs_list=`echo $$trs_list`; \
	$(MAKE) $(AM_MAKEFLAGS) $(TEST_SUITE_LOG) TEST_LOGS="$$log_list"; \
	exit $$?;
recheck: all $(check_PROGRAMS)
	@test -z "$(TEST_SUITE_LOG)" || rm -f $(TEST_SUITE_LOG)
	@set +e; $(am__set_TESTS_bases); \
	bases=`for i in $$bases; do echo $$i; done \
	         | $(am__list_recheck_tests)` || exit 1; \
	log_list=`for i in $$bases; do echo $$i.log; done`; \
	log_list=`echo $$log_list`; \
	$(MAKE) $(AM_MAKEFLAGS) $(TEST_SUITE_LOG) \
	        am__force_recheck=am--force-recheck \
	        TEST_LOGS="$$log_list"; \
	exit $$?
CUInt128Test.log: CUInt128Test$(EXEEXT)
	@p='CUInt128Test$(EXEEXT)'; \
	b='CUInt128Test'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
RangeMapTest.log: RangeMapTest$(EXEEXT)
	@p='RangeMapTest$(EXEEXT)'; \
	b='RangeMapTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
FormatTest.log: FormatTest$(EXEEXT)
	@p='FormatTest$(EXEEXT)'; \
	b='FormatTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
StringFunctionsTest.log: StringFunctionsTest$(EXEEXT)
	@p='StringFunctionsTest$(EXEEXT)'; \
	b='StringFunctionsTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
NetworkFunctionsTest.log: NetworkFunctionsTest$(EXEEXT)
	@p='NetworkFunctionsTest$(EXEEXT)'; \
	b='NetworkFunctionsTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
FileDataIOTest.log: FileDataIOTest$(EXEEXT)
	@p='FileDataIOTest$(EXEEXT)'; \
	b='FileDataIOTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
PathTest.log: PathTest$(EXEEXT)
	@p='PathTest$(EXEEXT)'; \
	b='PathTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
TextFileTest.log: TextFileTest$(EXEEXT)
	@p='TextFileTest$(EXEEXT)'; \
	b='TextFileTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
CTagTest.log: CTagTest$(EXEEXT)
	@p='CTagTest$(EXEEXT)'; \
	b='CTagTest'; \
	$(am__check_pre) $(LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_LOG_DRIVER_FLAGS) $(LOG_DRIVER_FLAGS) -- $(LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
.test.log:
	@p='$<'; \
	$(am__set_b); \
	$(am__check_pre) $(TEST_LOG_DRIVER) --test-name "$$f" \
	--log-file $$b.log --trs-file $$b.trs \
	$(am__common_driver_flags) $(AM_TEST_LOG_DRIVER_FLAGS) $(TEST_LOG_DRIVER_FLAGS) -- $(TEST_LOG_COMPILE) \
	"$$tst" $(AM_TESTS_FD_REDIRECT)
distdir: $(DISTFILES)
	@srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	topsrcdirstrip=`echo "$(top_srcdir)" | sed 's/[].[^$$\\*]/\\\\&/g'`; \
	list='$(DISTFILES)'; \
	  dist_files=`for file in $$list; do echo $$file; done | \
	  sed -e "s|^$$srcdirstrip/||;t" \
	      -e "s|^$$topsrcdirstrip/|$(top_builddir)/|;t"`; \
	case $$dist_files in \
	  */*) $(MKDIR_P) `echo "$$dist_files" | \
			   sed '/\//!d;s|^|$(distdir)/|;s,/[^/]*$$,,' | \
			   sort -u` ;; \
	esac; \
	for file in $$dist_files; do \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  if test -d $$d/$$file; then \
	    dir=`echo "/$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test -d "$(distdir)/$$file"; then \
	      find "$(distdir)/$$file" -type d ! -perm -700 -exec chmod u+rwx {} \;; \
	    fi; \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      cp -fpR $(srcdir)/$$file "$(distdir)$$dir" || exit 1; \
	      find "$(distdir)/$$file" -type d ! -perm -700 -exec chmod u+rwx {} \;; \
	    fi; \
	    cp -fpR $$d/$$file "$(distdir)$$dir" || exit 1; \
	  else \
	    test -f "$(distdir)/$$file" \
	    || cp -p $$d/$$file "$(distdir)/$$file" \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
	$(MAKE) $(AM_MAKEFLAGS) $(check_PROGRAMS)
	$(MAKE) $(AM_MAKEFLAGS) check-TESTS
check: check-am
all-am: Makefile
installdirs:
install: install-am
install-exec: install-exec-am
install-data: install-data-am
uninstall: uninstall-am
install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
installcheck: installcheck-am
install-strip:
	if test -z '$(STRIP)'; then \
	  $(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	    install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	      install; \
	else \
	  $(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	    install_sh_PROGRAM="$(INSTALL_STRIP_PROGRAM)" INSTALL_STRIP_FLAG=-s \
	    "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'" install; \
	fi
mostlyclean-generic:
	-test -z "$(TEST_LOGS)" || rm -f $(TEST_LOGS)
	-test -z "$(TEST_LOGS:.log=.trs)" || rm -f $(TEST_LOGS:.log=.trs)
	-test -z "$(TEST_SUITE_LOG)" || rm -f $(TEST_SUITE_LOG)
clean-generic:
distclean-generic:
	-test -z "$(CONFIG_CLEAN_FILES)" || rm -f $(CONFIG_CLEAN_FILES)
	-test . = "$(srcdir)" || test -z "$(CONFIG_CLEAN_VPATH_FILES)" || rm -f $(CONFIG_CLEAN_VPATH_FILES)
maintainer-clean-generic:
	@echo "This command is intended for maintainers to use"
	@echo "it deletes files that may require special tools to rebuild."
	-test -z "$(MAINTAINERCLEANFILES)" || rm -f $(MAINTAINERCLEANFILES)
clean: clean-am
clean-am: clean-checkPROGRAMS clean-generic mostlyclean-am
distclean: distclean-am
	-rm -rf ./$(DEPDIR)
	-rm -f Makefile
distclean-am: clean-am distclean-compile distclean-generic \
	distclean-tags
dvi: dvi-am
dvi-am:
html: html-am
html-am:
info: info-am
info-am:
install-data-am:
install-dvi: install-dvi-am
install-dvi-am:
install-exec-am:
install-html: install-html-am
install-html-am:
install-info: install-info-am
install-info-am:
install-man:
install-pdf: install-pdf-am
install-pdf-am:
install-ps: install-ps-am
install-ps-am:
installcheck-am:
maintainer-clean: maintainer-clean-am
	-rm -rf ./$(DEPDIR)
	-rm -f Makefile
maintainer-clean-am: distclean-am maintainer-clean-generic
mostlyclean: mostlyclean-am
mostlyclean-am: mostlyclean-compile mostlyclean-generic
pdf: pdf-am
pdf-am:
ps: ps-am
ps-am:
uninstall-am:
.MAKE: check-am install-am install-strip
.PHONY: CTAGS GTAGS TAGS all all-am check check-TESTS check-am clean \
	clean-checkPROGRAMS clean-generic cscopelist-am ctags ctags-am \
	distclean distclean-compile distclean-generic distclean-tags \
	distdir dvi dvi-am html html-am info info-am install \
	install-am install-data install-data-am install-dvi \
	install-dvi-am install-exec install-exec-am install-html \
	install-html-am install-info install-info-am install-man \
	install-pdf install-pdf-am install-ps install-ps-am \
	install-strip installcheck installcheck-am installdirs \
	maintainer-clean maintainer-clean-generic mostlyclean \
	mostlyclean-compile mostlyclean-generic pdf pdf-am ps ps-am \
	recheck tags tags-am uninstall uninstall-am
.PRECIOUS: Makefile
.NOEXPORT:
