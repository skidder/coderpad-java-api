package io.kidder.coderpad.response;

/**
 * @author Scott Kidder
 *
 */
public class BaseResponse {
    private String status;
    private String message;

    public String getStatus() {
	return status;
    }

    public void setStatus(String status) {
	this.status = status;
    }

    public String getMessage() {
	return message;
    }

    public void setMessage(String message) {
	this.message = message;
    }

    @Override
    public String toString() {
	return "BaseResponse [status=" + status + ", message=" + message + ", getStatus()=" + getStatus()
	       + ", getMessage()=" + getMessage() + ", getClass()=" + getClass() + ", hashCode()=" + hashCode()
	       + ", toString()=" + super.toString() + "]";
    }
}
