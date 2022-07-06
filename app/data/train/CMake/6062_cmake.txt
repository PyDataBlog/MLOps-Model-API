FILE(REMOVE_RECURSE
  "gnuradio_core_runtimePYTHON_wrap.cxx"
  "gnuradio_core_runtime.py"
  "gnuradio_core_generalPYTHON_wrap.cxx"
  "gnuradio_core_general.py"
  "gnuradio_core_gengenPYTHON_wrap.cxx"
  "gnuradio_core_gengen.py"
  "gnuradio_core_filterPYTHON_wrap.cxx"
  "gnuradio_core_filter.py"
  "gnuradio_core_ioPYTHON_wrap.cxx"
  "gnuradio_core_io.py"
  "gnuradio_core_hierPYTHON_wrap.cxx"
  "gnuradio_core_hier.py"
  "CMakeFiles/gnuradio_core_runtime_swig_doc"
  "runtime_swig_doc.i"
  "runtime_swig_doc_swig_docs/xml/index.xml"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/gnuradio_core_runtime_swig_doc.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
