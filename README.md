# idris-yaml

Basing on the Haskell [yaml][1] library, it is a wrapper fpr [libYAML][2].

# Status

At this moment, it's basically only FFI layer above `libYAML`, without even making use of its streaming capabilities.

# Building

TODO - clean the process up.

At this moment, it's required to checkout the `libYAML` submodule an follow its instructions to build the library.

The FFI layer uses own helpers, those can be built with the `make` command (it's specified in the ipkg file, so Idris should handle this.)

[1]: https://github.com/snoyberg/yaml/
[2]: http://pyyaml.org/wiki/LibYAML
