package io.kidder.coderpad.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.collect.Lists;

import io.kidder.coderpad.request.PadLanguage;

/**
 * @author Scott Kidder
 *
 */
public class PadResponse extends BaseResponse {

    private String id;
    private String title;
    private PadLanguage language;
    private List<String> participants = Lists.newArrayList();
    private String contents;
    private List<String> events = Lists.newArrayList();
    private boolean locked;
    @JsonProperty("private")
    private boolean privatePad;
    @JsonProperty("execution_enabled")
    private boolean executionEnabled;
    @JsonProperty("created_at")
    private Date createdAt;
    @JsonProperty("updated_at")
    private Date updatedAt;
    @JsonProperty("url")
    private String editingUrl;
    @JsonProperty("playback")
    private String playbackUrl;
    @JsonProperty("history")
    private String historyUrl;
    @JsonProperty("hangout")
    private String hangoutUrl;

    public String getId() {
	return id;
    }

    public void setId(String id) {
	this.id = id;
    }

    public String getTitle() {
	return title;
    }

    public void setTitle(String title) {
	this.title = title;
    }

    public PadLanguage getLanguage() {
	return language;
    }

    public void setLanguage(PadLanguage language) {
	this.language = language;
    }

    public List<String> getParticipants() {
	return participants;
    }

    public void setParticipants(List<String> participants) {
	this.participants = participants;
    }

    public String getContents() {
	return contents;
    }

    public void setContents(String contents) {
	this.contents = contents;
    }

    public List<String> getEvents() {
	return events;
    }

    public void setEvents(List<String> events) {
	this.events = events;
    }

    public boolean isLocked() {
	return locked;
    }

    public void setLocked(boolean locked) {
	this.locked = locked;
    }

    public boolean isPrivatePad() {
	return privatePad;
    }

    public void setPrivatePad(boolean privatePad) {
	this.privatePad = privatePad;
    }

    public boolean isExecutionEnabled() {
	return executionEnabled;
    }

    public void setExecutionEnabled(boolean executionEnabled) {
	this.executionEnabled = executionEnabled;
    }

    public Date getCreatedAt() {
	return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
	this.createdAt = createdAt;
    }

    public Date getUpdatedAt() {
	return updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
	this.updatedAt = updatedAt;
    }

    public String getEditingUrl() {
	return editingUrl;
    }

    public void setEditingUrl(String editingUrl) {
	this.editingUrl = editingUrl;
    }

    public String getPlaybackUrl() {
	return playbackUrl;
    }

    public void setPlaybackUrl(String playbackUrl) {
	this.playbackUrl = playbackUrl;
    }

    public String getHistoryUrl() {
	return historyUrl;
    }

    public void setHistoryUrl(String historyUrl) {
	this.historyUrl = historyUrl;
    }

    public String getHangoutUrl() {
	return hangoutUrl;
    }

    public void setHangoutUrl(String hangoutUrl) {
	this.hangoutUrl = hangoutUrl;
    }

    @Override
    public String toString() {
	return "PadResponse [id=" + id + ", title=" + title + ", language=" + language + ", participants="
		+ participants + ", contents=" + contents + ", events=" + events + ", locked=" + locked
		+ ", privatePad=" + privatePad + ", executionEnabled=" + executionEnabled + ", createdAt=" + createdAt
		+ ", updatedAt=" + updatedAt + ", editingUrl=" + editingUrl + ", playbackUrl=" + playbackUrl
		+ ", historyUrl=" + historyUrl + ", hangoutUrl=" + hangoutUrl + ", status=" + getStatus() + ", message="
		+ getMessage() + "]";
    }
}
  