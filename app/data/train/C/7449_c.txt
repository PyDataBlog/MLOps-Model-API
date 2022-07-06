/* ========================================================================= */
/* ------------------------------------------------------------------------- */
/*!
  \file			template.c
  \date			March 2014
  \author		Nicu Tofan
  
  \brief		implementation of a template
  
  
*/
/* ------------------------------------------------------------------------- */
/* ========================================================================= */
//
//
//
//
/*  INCLUDES    ------------------------------------------------------------ */

#include "logic.h"
#include "template.h"

#include <string.h>
#include <stdlib.h>

/*  INCLUDES    ============================================================ */
//
//
//
//
/*  DEFINITIONS    --------------------------------------------------------- */

static int logic_template_unchain (logic_template_t *item);

/*  DEFINITIONS    ========================================================= */
//
//
//
//
/*  DATA    ---------------------------------------------------------------- */

/*  DATA    ================================================================ */
//
//
//
//
/*  FUNCTIONS    ----------------------------------------------------------- */

LOGIC_EXPORT
func_exit_code_t logic_template_new (
        xpotepu_logic_t * parent, logic_template_t **tpl)
{
    DBG_ASSERT(parent != NULL);
    DBG_ASSERT(tpl != NULL);
    
    logic_template_t * item = NULL;
    func_exit_code_t ret = FUNC_OK;
    for (;;) {
        
        // allocate and clear
        item = (logic_template_t*)malloc (sizeof(logic_template_t));
        if (item == NULL) {
            ret = FUNC_MEMORY_ERROR;
            break;
        }
        memset (item, 0, sizeof(logic_template_t));
        
        // set known members
        item->parent_ = parent;
        

        // insert it in the list
        item->next_ = parent->tpl_;
        parent->tpl_ = item;
        
        
        break;
    }
    
    *tpl = item;
    return ret;
}

LOGIC_EXPORT
void logic_template_free (logic_template_t **tpl)
{
    
    logic_template_t * item = *tpl;
    if (item != NULL) {
        
        // extract and clear
        logic_template_unchain (item);        
        memset (item, 0, sizeof(logic_template_t));
        
        // release it
        free (item);
        *tpl = NULL;
    }
}

static int logic_template_unchain (logic_template_t *item)
{
    int ret = 0;
    
    // extract it from the chain
    if (item->parent_ != NULL) {
        logic_template_t * prev = NULL;
        logic_template_t * iter = item->parent_->tpl_;
        while (iter != NULL) {
            if (iter == item) {
                if (prev == NULL) {
                    item->parent_->tpl_ = item->next_;
                } else {
                    prev->next_ = item->next_;
                }
                ret = 1;
                break;
            }
            prev = iter;
            iter = iter->next_;
        }
    }
    
    return ret;
}


/*  FUNCTIONS    =========================================================== */
//
//
//
//
/* ------------------------------------------------------------------------- */
/* ========================================================================= */
