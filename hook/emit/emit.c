#include <stdio.h>
#include "ubik/natives.h"
#include "ubik/rt.h"
#include "ubik/schedule.h"

ubik_error
eval_emit(struct ubik_exec_graph *gexec)
{
        /* this uses fwrite to remove the requirement for a NULL byte at the end
           of the string. */
        fwrite(gexec->nv[0]->str.data, sizeof(char),
               gexec->nv[0]->str.length, stdout);

        gexec->nv[1] = gexec->nv[0];
        gexec->nt[1] = gexec->nt[0];

        return OK;
}

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_native_record *r;
        ubik_alloc1(&r, struct ubik_native_record, region);
        r->name = "emit";
        r->arity = 1;
        r->type_string = "String -> String";
        r->eval = eval_emit;
        return ubik_vector_append(hooks, r);
}
