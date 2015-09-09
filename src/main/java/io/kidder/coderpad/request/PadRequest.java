package io.kidder.coderpad.request;

/**
 * @author Scott Kidder
 *
 */
public class PadRequest {
    private String title;
    private PadLanguage language;
    private String contents;
    private boolean locked = false;
    private boolean privatePad = false;
    private boolean executionEnabled = true;

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

    public String getContents() {
	return contents;
    }

    public void setContents(String contents) {
	this.contents = contents;
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

    @Override
    public String toString() {
	return "PadRequest [title=" + title + ", language=" + language + ", contents=" + contents + ", locked="
	       + locked + ", privatePad=" + privatePad + ", executionEnabled=" + executionEnabled + "]";
    }
}
