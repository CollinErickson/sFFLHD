The previous version doesn't work since the entire DoE.base
package needs to be loaded. So I moved it to from 
Imports to Depends.
I don't want to do this since I only directly call one
function from the package, but it's the only way to get it
to work now. I have emailed the maintainer of DoE.base seeing
if they have plans to make the single function work without
needing to load the entire package and am awaiting their reply.

## Test environments
* local Windows 7 install, R 3.3.1
* Linux on a UNIX cluster, R 3.1.2
* win-builder

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.
