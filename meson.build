project('parsetcl', 'c',
  version:          '0.1.3',
  default_options:  ['c_std=gnu17', 'warning_level=3', 'werror=true'],
  meson_version:    '>=1.3.0',
)

cc = meson.get_compiler('c')
fs = import('fs')
add_project_arguments('-D_DEFAULT_SOURCE', language: 'c')

if get_option('debug')
  add_project_arguments(cc.get_supported_arguments(['-ggdb3']), language: 'c')
endif

# Suppress warnings that TEA-style code commonly triggers
add_project_arguments(
  '-Wno-missing-field-initializers',
  '-Wno-unused-parameter',     # interp / incmdsubst params held for symmetry
  language: 'c',
)

# --- Configuration data (becomes config.h) ---
conf = configuration_data()

# TIP445 shim for Tcl 8.6 compat
tea_want_shims = ['tip445']

# --- Accumulator variables for teabase ---
deps = []
inc = []
teaconf_files = []

# --- Package sources ---
inc += include_directories('generic')
pkg_sources = files(
  'generic/parsetcl.c',
  'generic/obstack_pool.c',
)

teaconf_files += files(
  'library/parser.tcl',
  'library/parser_utils.tcl',
)

# --- Teabase init (must come first) ---
# Provides: pkg_dir, tclsh, tcl_dep, teaconf
subdir('teabase/init')

# --- tdom dependency (for parsing into a tDOM document) ---
# Prefer pkg-config (tdom.pc lands in tcl_dep's libdir/pkgconfig when tdom
# is installed via its own meson build).  Otherwise fall back to building
# tdom as a subproject and consume its in-tree stubs library directly —
# this works without tdom needing to call meson.override_dependency.
tea_test_deps = []

tdom_dep = dependency('tdom', required: false)
if not tdom_dep.found()
  # html5: disabled — we don't parse HTML, so the optional libgumbo dep
  # (and its LTO-version-pinned binary on the reference Tcl install) is
  # just dead weight.
  tdom_proj = subproject('tdom', default_options: ['html5=disabled'])
  tdom_dep = declare_dependency(
    link_with:           tdom_proj.get_variable('stubs_lib'),
    include_directories: tdom_proj.get_variable('inc'),
    compile_args:        ['-DUSE_TDOM_STUBS=1'],
  )
  # Make tdom's runtime shared module loadable by the test interpreter.
  tea_test_deps += tdom_proj.get_variable('pkg_so')
endif
deps += tdom_dep

# --- Teabase main (builds shared_module, sets up tests, devenv) ---
subdir('teabase')
