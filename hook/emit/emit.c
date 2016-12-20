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

size_t
__ubik_hooks_count = 1;

struct ubik_native_record
__ubik_hooks[] = {
        { "emit", "String -> String", NULL, eval_emit },
};
