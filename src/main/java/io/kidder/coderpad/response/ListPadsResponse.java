package io.kidder.coderpad.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.collect.Lists;

public class ListPadsResponse {
    private String status;
    private List<PadResponse> pads = Lists.newArrayList();
    @JsonProperty("next_page")
    private String nextPageUrl;
    private long total;

    public String getStatus() {
	return status;
    }

    public void setStatus(String status) {
	this.status = status;
    }

    public List<PadResponse> getPads() {
	return pads;
    }

    public void setPads(List<PadResponse> pads) {
	this.pads = pads;
    }

    public String getNextPageUrl() {
	return nextPageUrl;
    }

    public void setNextPageUrl(String nextPageUrl) {
	this.nextPageUrl = nextPageUrl;
    }

    public long getTotal() {
	return total;
    }

    public void setTotal(long total) {
	this.total = total;
    }

    @Override
    public String toString() {
	return "ListPadsResponse [status=" + status + ", pads=" + pads + ", nextPageUrl=" + nextPageUrl + ", total="
	       + total + "]";
    }
}
