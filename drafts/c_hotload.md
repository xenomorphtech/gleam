```
global {
 modules_index : [name, num],
 modules_all: [ptr]
} 
module {
 functions -> [name, ptr]
}
get_external_func(module, name)
```

when the module loads, resolves all its deps, a module like genserv can do get_external_func(module, name)

import m;
static m = get_module("m");

