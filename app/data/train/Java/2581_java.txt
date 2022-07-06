package com.percolate.sdk.dto;

import com.fasterxml.jackson.annotation.*;
import com.percolate.sdk.interfaces.HasExtraFields;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("UnusedDeclaration")
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FacebookConversationMessage implements Serializable, HasExtraFields {

    private static final long serialVersionUID = -7978616001157824820L;

    @JsonIgnore
    public List<FacebookConversationMessage> extraMessages = new ArrayList<>(); // Set by client to group messages happening around the same time

    @JsonIgnore
    public String stickerUrl; //Set by client.  If message is empty, ApiGetFacebookMessage is used to check for images/stickers.

    @JsonIgnore
    public List<FacebookMessageAttachment> attachments; //Set by client after calling ApiGetFacebookMessage.

    @JsonIgnore
    public Flag flag; //Set by client after calling ApiGetFlags

    @JsonProperty("id")
    protected String id;

    @JsonProperty("conversation_id")
    protected String conversationId;

    @JsonProperty("from")
    protected FacebookUser from;

    // ApiGetFacebookConversations API returns "from"
    // ApiGetFlag API returns "from_user"
    // The setter method for this value sets <code>from</code> field.
    @JsonProperty("from_user")
    protected FacebookUser tempFromUser;

    @JsonProperty("created_at")
    protected String createdAt;

    @JsonProperty("has_attachments")
    protected Boolean hasAttachments;

    @JsonProperty("message")
    protected String message;

    @JsonIgnore
    protected Map<String, Object> extraFields = new HashMap<>();

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getConversationId() {
        return conversationId;
    }

    public void setConversationId(String conversationId) {
        this.conversationId = conversationId;
    }

    public FacebookUser getFrom() {
        return from;
    }

    public void setFrom(FacebookUser from) {
        this.from = from;
    }

    public void setTempFromUser(FacebookUser from) {
        this.tempFromUser = from;
        this.from = from;
    }

    public FacebookUser getTempFromUser() {
        return tempFromUser;
    }

    public String getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(String createdAt) {
        this.createdAt = createdAt;
    }

    public Boolean getHasAttachments() {
        return hasAttachments;
    }

    public void setHasAttachments(Boolean hasAttachments) {
        this.hasAttachments = hasAttachments;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }


    @Override
    public Map<String, Object> getExtraFields() {
        if(extraFields == null) {
            extraFields = new HashMap<>();
        }
        return extraFields;
    }

    @Override
    @JsonAnySetter
    public void putExtraField(String key, Object value) {
        getExtraFields().put(key, value);
    }
}
