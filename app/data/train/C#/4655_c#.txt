/* **********************************************************************************************************
 * The MIT License (MIT)																					*
 * 																											*
 * Copyright (c) 2016 Hypermediasystems Ges. f. Software mbH										*
 * Web: http://www.hypermediasystems.de																		*
 * This file is part of hmsspx																			*
 * 																											*
 * Permission is hereby granted, free of charge, to any person obtaining a copy								*
 * of this software and associated documentation files (the "Software"), to deal							*
 * in the Software without restriction, including without limitation the rights								*
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell								*
 * copies of the Software, and to permit persons to whom the Software is									*
 * furnished to do so, subject to the following conditions:													*
 * 																											*
 * The above copyright notice and this permission notice shall be included in								*
 * all copies or substantial portions of the Software.														*
 * 																											*
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR								*
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,									*
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE								*
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER									*
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,							*
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN								*
 * THE SOFTWARE.																							*
 ************************************************************************************************************ */ 
using System;
namespace hmsspx
{
    partial class pageBase
    {
        public int add_RelatedField(int level, HMS.SP.RelatedField spRelatedField)
        {
            return add_RelatedField(-1, level, spRelatedField);
        }
        public int  add_RelatedField(int pos, int level, HMS.SP.RelatedField spRelatedField)
        {
            if (spRelatedField.__HMSError_ != null && (spRelatedField.__HMSError_.errno != 0 || spRelatedField.__HMSError_.info != "" ))
            {
                pos = this.add_lineAt(pos, level, "__HMSError", HMS.Util.__HMSError.toString(spRelatedField.__HMSError_)); 
            }
            pos = this.add__deferred(pos, level, spRelatedField.__deferred_);
            pos = this.add__metadata(pos, level, spRelatedField.__metadata_);
            pos = this.add__rest(pos, level, spRelatedField.__rest);
            pos = this.add_lineAt(pos,level,  "FieldId", HMS.SP.SPUtil.toString(spRelatedField.FieldId_)); //Guid;
            pos = this.add_lineAt(pos,level,  "ListId", HMS.SP.SPUtil.toString(spRelatedField.ListId_)); //Guid;
            pos = this.add_lineAt(pos,level,  "LookupList", HMS.SP.SPUtil.toString(spRelatedField.LookupList_)); //String;
            pos = this.add_lineAt(pos,level,  "RelationshipDeleteBehavior", HMS.SP.SPUtil.toString(spRelatedField.RelationshipDeleteBehavior_)); //Object;
            pos = this.add_lineAt(pos,level,  "WebId", HMS.SP.SPUtil.toString(spRelatedField.WebId_)); //Guid;
            return pos;
        }
        public int add_RelatedField_deferred(int pos, int level, HMS.SP.RelatedField spRelatedField)
        {
            {
                string useUrl = spRelatedField.__deferred_.uri;
                if( !String.IsNullOrEmpty(useUrl) )
                {
                    pos = this.add_lineAt(pos, level, false,  "RelatedField","", knownPages.pageRelatedField, useUrl);
                }
                else
                {
                    pos = this.add_lineAt(pos, level, true,   "RelatedField", "",knownPages.NONE, "");
                    pos = this.add_RelatedField(pos, level+1, spRelatedField);
                }
            }
        return pos;
        }
    }
}
