project('parsetcl', 'c',
  version:          '0.3',
  default_options:  ['c_std=gnu2y,gnu23,gnu2x', 'warning_level=3', 'werror=true'],
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
  cc.get_supported_arguments([
    '-Wno-missing-field-initializers',
    '-Wno-attributes',
    '-Wno-deprecated-octal-literals',   # TCL_SUBST_* defined in tcl.h as octal without `0o` prefix
  ]),
  language: 'c',
)

# --- Configuration data (becomes config.h) ---
conf = configuration_data()

# TIP445 shim for Tcl 8.6 compat
tea_want_shims = ['tip445']
tea_want_defer = true

# --- Accumulator variables for teabase ---
deps = []
inc = []
teaconf_files = []

# --- Package sources ---
inc += include_directories('.')
pkg_sources = files(
  'parsetcl.c',
  'obstack_pool.c',
)

teaconf_files += files(
  'parser.tcl',
  'parser_utils.tcl',
)

# --- Teabase init (must come first) ---
# Provides: pkg_dir, tclsh, tcl_dep, teaconf
#subdir('teabase/init')

# --- tdom dependency (for parsing into a tDOM document) ---
# Prefer pkg-config (tdom.pc lands in tcl_dep's libdir/pkgconfig when tdom
# is installed via its own meson build).  Otherwise fall back to building
# tdom as a subproject and consume its in-tree stubs library directly —
# this works without tdom needing to call meson.override_dependency.
deps += dependency('tdom',
  version:          '>=0.9.6.4',
  default_options:  ['html5=disabled'],
)

# --- Teabase main (builds shared_module, sets up tests, devenv) ---
subdir('teabase')

# --- Documentation (manpage + README via pandoc, optional) ---
subdir('doc')
