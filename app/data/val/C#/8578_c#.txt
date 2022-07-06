/* **********************************************************************************************************
 * The MIT License (MIT)																					*
 * 																											*
 * Copyright (c) 2016 Hypermediasystems Ges. f. Software mbH										*
 * Web: http://www.hypermediasystems.de																		*
 * This file is part of hmssp																			*
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
using System.Collections.Generic;
using System.Dynamic;
using System.Reflection;
using Newtonsoft.Json;
namespace HMS.SP{
    /// <summary>
    /// <para>https://msdn.microsoft.com/en-us/library/office/dn531433.aspx#bk_ListItem</para>
    /// </summary>
    public class ListItem : SPBase{
        [JsonProperty("__HMSError")]
        public HMS.Util.__HMSError __HMSError_ { set; get; }
        [JsonProperty("__status")]
        public SP.__status __status_ { set; get; }
        [JsonProperty("__deferred")]
        public SP.__deferred __deferred_ { set; get; }
        [JsonProperty("__metadata")]
        public SP.__metadata __metadata_ { set; get; }
        public Dictionary<string, string> __rest;
        /// <summary>
        /// <para>Specifies the collection of attachments that are associated with the list item.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("AttachmentFiles")]
        public SP.AttachmentCollection AttachmentFiles_ { set; get; }
        /// <summary>
        /// <para>Gets a value that specifies the content type of the list item.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("ContentType")]
        public SP.ContentType ContentType_ { set; get; }
        /// <summary>
        /// <para>Gets a value that specifies the display name of the list item.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:Yes (displayed as the "Title" value)</para>
        /// </summary>
        [JsonProperty("DisplayName")]
        public String DisplayName_ { set; get; }
        /// <summary>
        /// <para>Gets a value that specifies the effective permissions on the list item that are assigned to the current user.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:Yes</para>
        /// </summary>
        [JsonProperty("EffectiveBasePermissions")]
        public SP.BasePermissions EffectiveBasePermissions_ { set; get; }
        /// <summary>
        /// <para>Gets the effective base permissions for the current user, as they should be displayed in UI.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:Yes</para>
        /// </summary>
        [JsonProperty("EffectiveBasePermissionsForUI")]
        public SP.BasePermissions EffectiveBasePermissionsForUI_ { set; get; }
        /// <summary>
        /// <para>Gets the values for the list item as HTML.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("FieldValuesAsHtml")]
        public SP.FieldStringValues FieldValuesAsHtml_ { set; get; }
        /// <summary>
        /// <para>Gets the list item's field values as a collection of string values.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("FieldValuesAsText")]
        public SP.FieldStringValues FieldValuesAsText_ { set; get; }
        /// <summary>
        /// <para>Gets the formatted values to be displayed in an edit form.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("FieldValuesForEdit")]
        public SP.FieldStringValues FieldValuesForEdit_ { set; get; }
        /// <summary>
        /// <para>Gets the file that is represented by the item from a document library.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("File")]
        public SP.File File_ { set; get; }
        /// <summary>
        /// <para>Gets a value that specifies whether the list item is a file or a list folder. Represents an SP.FileSystemObjectType value: Invalid = -1; File = 0; Folder = 1; Web = 2.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:Yes</para>
        /// </summary>
        [JsonProperty("FileSystemObjectType")]
        public Int32 FileSystemObjectType_ { set; get; }
        /// <summary>
        /// <para>Gets the object where role assignments for this object are defined. If role assignments are defined directly on the current object, the current object is returned.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("FirstUniqueAncestorSecurableObject")]
        public SP.SecurableObject FirstUniqueAncestorSecurableObject_ { set; get; }
        /// <summary>
        /// <para>Gets a folder object that is associated with a folder item.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("Folder")]
        public SP.Folder Folder_ { set; get; }
        /// <summary>
        /// <para>Gets a value that specifies whether the role assignments are uniquely defined for this securable object or inherited from a parent securable object.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("HasUniqueRoleAssignments")]
        public Boolean HasUniqueRoleAssignments_ { set; get; }
        /// <summary>
        /// <para>Gets a value that specifies the list item identifier.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:Yes</para>
        /// </summary>
        [JsonProperty("Id")]
        public Int32 Id_ { set; get; }
        /// <summary>
        /// <para>Gets the parent list that contains the list item.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("ParentList")]
        public SP.List ParentList_ { set; get; }
        /// <summary>
        /// <para>Gets the role assignments for the securable object.</para>
        /// <para>R/W: R</para>
        /// <para>Returned with resource:No</para>
        /// </summary>
        [JsonProperty("RoleAssignments")]
        public SP.RoleAssignmentCollection RoleAssignments_ { set; get; }
        /// <summary>
        /// <para> Endpoints </para>
        /// </summary>
        static string[] endpoints = {
            "http://<site url>/web/lists(guid'<list id>')/items(<item id>)",
            "http://<site url>/web/lists/getbytitle('<list title>')/items(<item id>)",
        };

        public ListItem(ExpandoObject expObj)
        {
             try
             {
                var use_EO = ((dynamic)expObj).entry.content.properties;
                HMS.SP.SPUtil.expando2obj(use_EO, this, typeof(ListItem));
             }
             catch (Exception ex)
             {
             
             }
        
        }
        // used by Newtonsoft.JSON 
        public  ListItem()
        {
        }
        public  ListItem(string json)
        {
            if( json == String.Empty )
                return;
            dynamic jobject = Newtonsoft.Json.JsonConvert.DeserializeObject(json);            
            dynamic refObj = jobject;
            if (jobject.d != null)
                refObj = jobject.d;
            string errInfo = "";
            if (refObj.results != null)
            {
                if (refObj.results.Count > 1)
                    errInfo = "Result is Collection, only 1. entry displayed.";
                refObj = refObj.results[0];
            }
            List<string> usedFields = new List<string>();
                usedFields.Add("__HMSError");
            HMS.SP.SPUtil.dyn_ValueSet("__HMSError", refObj, this);
                usedFields.Add("__deferred");
            this.__deferred_ = new SP.__deferred(HMS.SP.SPUtil.dyn_toString(refObj.__deferred));
                usedFields.Add("__metadata");
            this.__metadata_ = new SP.__metadata(HMS.SP.SPUtil.dyn_toString(refObj.__metadata));
                usedFields.Add("AttachmentFiles");
            this.AttachmentFiles_ = new SP.AttachmentCollection(HMS.SP.SPUtil.dyn_toString(refObj.AttachmentFiles));
                usedFields.Add("ContentType");
            this.ContentType_ = new SP.ContentType(HMS.SP.SPUtil.dyn_toString(refObj.ContentType));
                usedFields.Add("DisplayName");
            HMS.SP.SPUtil.dyn_ValueSet("DisplayName", refObj, this);
                usedFields.Add("EffectiveBasePermissions");
            this.EffectiveBasePermissions_ = new SP.BasePermissions(HMS.SP.SPUtil.dyn_toString(refObj.EffectiveBasePermissions));
                usedFields.Add("EffectiveBasePermissionsForUI");
            this.EffectiveBasePermissionsForUI_ = new SP.BasePermissions(HMS.SP.SPUtil.dyn_toString(refObj.EffectiveBasePermissionsForUI));
                usedFields.Add("FieldValuesAsHtml");
            this.FieldValuesAsHtml_ = new SP.FieldStringValues(HMS.SP.SPUtil.dyn_toString(refObj.FieldValuesAsHtml));
                usedFields.Add("FieldValuesAsText");
            this.FieldValuesAsText_ = new SP.FieldStringValues(HMS.SP.SPUtil.dyn_toString(refObj.FieldValuesAsText));
                usedFields.Add("FieldValuesForEdit");
            this.FieldValuesForEdit_ = new SP.FieldStringValues(HMS.SP.SPUtil.dyn_toString(refObj.FieldValuesForEdit));
                usedFields.Add("File");
            this.File_ = new SP.File(HMS.SP.SPUtil.dyn_toString(refObj.File));
                usedFields.Add("FileSystemObjectType");
            HMS.SP.SPUtil.dyn_ValueSet("FileSystemObjectType", refObj, this);
                usedFields.Add("FirstUniqueAncestorSecurableObject");
            this.FirstUniqueAncestorSecurableObject_ = new SP.SecurableObject(HMS.SP.SPUtil.dyn_toString(refObj.FirstUniqueAncestorSecurableObject));
                usedFields.Add("Folder");
            this.Folder_ = new SP.Folder(HMS.SP.SPUtil.dyn_toString(refObj.Folder));
                usedFields.Add("HasUniqueRoleAssignments");
            HMS.SP.SPUtil.dyn_ValueSet("HasUniqueRoleAssignments", refObj, this);
                usedFields.Add("Id");
            HMS.SP.SPUtil.dyn_ValueSet("Id", refObj, this);
                usedFields.Add("ParentList");
            this.ParentList_ = new SP.List(HMS.SP.SPUtil.dyn_toString(refObj.ParentList));
                usedFields.Add("RoleAssignments");
            this.RoleAssignments_ = new SP.RoleAssignmentCollection(HMS.SP.SPUtil.dyn_toString(refObj.RoleAssignments));
            this.__rest = new Dictionary<string, string>();
            var dyn = ((Newtonsoft.Json.Linq.JContainer)refObj).First;
            while (dyn != null)
            {
                string Name = ((Newtonsoft.Json.Linq.JProperty)dyn).Name;
                string Value = ((Newtonsoft.Json.Linq.JProperty)dyn).Value.ToString();
                if ( !usedFields.Contains( Name ))
                    this.__rest.Add( Name, Value);
                dyn = dyn.Next;
            }
            if( errInfo != "")
                this.__HMSError_.info = errInfo;
        }
    }
}
