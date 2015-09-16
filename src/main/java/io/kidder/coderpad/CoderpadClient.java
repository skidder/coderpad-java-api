/**
 * 
 */
package io.kidder.coderpad;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.kidder.coderpad.request.ListPadsSortingOrder;
import io.kidder.coderpad.request.ListPadsSortingTerm;
import io.kidder.coderpad.request.PadRequest;
import io.kidder.coderpad.response.BaseResponse;
import io.kidder.coderpad.response.ListPadsResponse;
import io.kidder.coderpad.response.PadResponse;

/**
 * Client for the Coderpad API.
 * 
 * @author Scott Kidder
 *
 */
public class CoderpadClient {

    private static final String OK_STATUS = "OK";
    private static final String AUTHORIZATION_HEADER = "Authorization";
    private static final String CODERPAD_BASE_URL = "https://coderpad.io/api";
    private String authenticationToken;
    private String baseUrl;
    private ObjectMapper objectMapper;

    public CoderpadClient(String authenticationToken) {
	this(authenticationToken, CODERPAD_BASE_URL);
    }

    public CoderpadClient(String authenticationToken, String baseUrl) {
	this.authenticationToken = authenticationToken;
	this.baseUrl = baseUrl;
	objectMapper = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * Get a pad resource by its ID
     * 
     * @param padId
     * @return
     * @throws CoderpadException
     */
    public PadResponse getPad(String padId) throws CoderpadException {
	final HttpGet getRequest = new HttpGet(this.baseUrl + "/pads/" + padId);
	getRequest.addHeader(AUTHORIZATION_HEADER, generateTokenHeaderValue());
	getRequest.setHeader("Accept", "application/json");

	CloseableHttpClient httpclient = HttpClients.createDefault();
	CloseableHttpResponse getResponse = null;
	try {
	    getResponse = httpclient.execute(getRequest);
	    final HttpEntity responseEntity = getResponse.getEntity();
	    if (responseEntity != null) {
		final PadResponse getPadResponse = objectMapper.readValue(responseEntity.getContent(),
			PadResponse.class);
		if (!OK_STATUS.equals(getPadResponse.getStatus())) {
		    throw new CoderpadException(getPadResponse.getMessage());
		}
		return getPadResponse;
	    } else {
		throw new CoderpadException("Empty response from Coderpad");
	    }
	} catch (Exception e) {
	    throw new CoderpadException("Error getting pad on Coderpad", e);
	} finally {
	    try {
		if (getResponse != null)
		    getResponse.close();
	    } catch (IOException e) {
		// ignore
	    }
	}
    }

    /**
     * List pads using the default sort order (created_at,desc).
     * 
     * @return
     * @throws CoderpadException
     */
    public ListPadsResponse listPads() throws CoderpadException {
	final HttpGet getRequest = new HttpGet(this.baseUrl + "/pads/");
	getRequest.addHeader(AUTHORIZATION_HEADER, generateTokenHeaderValue());
	getRequest.setHeader("Accept", "application/json");

	CloseableHttpClient httpclient = HttpClients.createDefault();
	CloseableHttpResponse getResponse = null;
	try {
	    getResponse = httpclient.execute(getRequest);
	    final HttpEntity responseEntity = getResponse.getEntity();
	    if (responseEntity != null) {
		final ListPadsResponse listPadsResponse = objectMapper.readValue(responseEntity.getContent(),
			ListPadsResponse.class);
		if (!OK_STATUS.equals(listPadsResponse.getStatus())) {
		    throw new CoderpadException(listPadsResponse.getMessage());
		}
		return listPadsResponse;
	    } else {
		throw new CoderpadException("Empty response from Coderpad");
	    }
	} catch (Exception e) {
	    throw new CoderpadException("Error listing pads on Coderpad", e);
	} finally {
	    try {
		if (getResponse != null)
		    getResponse.close();
	    } catch (IOException e) {
		// ignore
	    }
	}
    }

    /**
     * Create a new pad with the attributes specified in the request.
     * 
     * @param request
     * @return
     * @throws CoderpadException
     */
    public PadResponse createPad(PadRequest request) throws CoderpadException {
	final List<NameValuePair> form = createHttpFormForPadRequest(request);
	final UrlEncodedFormEntity entity = new UrlEncodedFormEntity(form, Consts.UTF_8);
	final HttpPost postRequest = new HttpPost(this.baseUrl + "/pads");
	postRequest.setEntity(entity);
	postRequest.addHeader(AUTHORIZATION_HEADER, generateTokenHeaderValue());
	postRequest.setHeader("Accept", "application/json");

	CloseableHttpClient httpclient = HttpClients.createDefault();
	CloseableHttpResponse postResponse = null;
	try {
	    postResponse = httpclient.execute(postRequest);
	    final HttpEntity responseEntity = postResponse.getEntity();
	    if (responseEntity != null) {
		final PadResponse padResponse = objectMapper.readValue(responseEntity.getContent(), PadResponse.class);
		if (!OK_STATUS.equals(padResponse.getStatus())) {
		    throw new CoderpadException(padResponse.getMessage());
		}
		return padResponse;
	    } else {
		throw new CoderpadException("Empty response from Coderpad");
	    }
	} catch (Exception e) {
	    throw new CoderpadException("Error creating pad on Coderpad", e);
	} finally {
	    try {
		if (postResponse != null)
		    postResponse.close();
	    } catch (IOException e) {
		// ignore
	    }
	}
    }

    /**
     * Update an existing pad.
     * 
     * @param id
     * @param request
     * @throws CoderpadException
     */
    public void updatePad(String id, PadRequest request) throws CoderpadException {
	final List<NameValuePair> form = createHttpFormForPadRequest(request);
	final UrlEncodedFormEntity entity = new UrlEncodedFormEntity(form, Consts.UTF_8);
	final HttpPut putRequest = new HttpPut(this.baseUrl + "/pads");
	putRequest.setEntity(entity);
	putRequest.addHeader(AUTHORIZATION_HEADER, generateTokenHeaderValue());
	putRequest.setHeader("Accept", "application/json");

	CloseableHttpClient httpclient = HttpClients.createDefault();
	CloseableHttpResponse putResponse = null;
	try {
	    putResponse = httpclient.execute(putRequest);
	    final HttpEntity responseEntity = putResponse.getEntity();
	    if (responseEntity != null) {
		final BaseResponse padResponse = objectMapper.readValue(responseEntity.getContent(),
			BaseResponse.class);
		if (!OK_STATUS.equals(padResponse.getStatus())) {
		    throw new CoderpadException(padResponse.getMessage());
		}
	    } else {
		throw new CoderpadException("Empty response from Coderpad while updating pad");
	    }
	} catch (Exception e) {
	    throw new CoderpadException("Error updating pad", e);
	} finally {
	    try {
		if (putResponse != null)
		    putResponse.close();
	    } catch (IOException e) {
		// ignore
	    }
	}
    }

    /**
     * Delete an existing pad.
     * 
     * @param id
     * @throws CoderpadException
     */
    public void deletePad(String padId) throws CoderpadException {
	final HttpDelete deleteRequest = new HttpDelete(this.baseUrl + "/pads/" + padId);
	deleteRequest.addHeader(AUTHORIZATION_HEADER, generateTokenHeaderValue());
	deleteRequest.setHeader("Accept", "application/json");

	CloseableHttpClient httpclient = HttpClients.createDefault();
	CloseableHttpResponse deleteResponse = null;
	try {
	    deleteResponse = httpclient.execute(deleteRequest);
	    final HttpEntity responseEntity = deleteResponse.getEntity();
	    if (responseEntity != null) {
		final PadResponse getPadResponse = objectMapper.readValue(responseEntity.getContent(),
			PadResponse.class);
		if (!OK_STATUS.equals(getPadResponse.getStatus())) {
		    throw new CoderpadException(getPadResponse.getMessage());
		}
	    } else {
		throw new CoderpadException("Empty response from Coderpad");
	    }
	} catch (Exception e) {
	    throw new CoderpadException("Error getting pad on Coderpad", e);
	} finally {
	    try {
		if (deleteResponse != null)
		    deleteResponse.close();
	    } catch (IOException e) {
		// ignore
	    }
	}
    }

    /**
     * List pads using the specified sorting term and method.
     * 
     * @param sortingTerm
     * @param sortingOrder
     * @return
     * @throws CoderpadException
     */
    public ListPadsResponse listPads(ListPadsSortingTerm sortingTerm, ListPadsSortingOrder sortingOrder)
	    throws CoderpadException {
	if (sortingTerm == null || sortingOrder == null) {
	    throw new IllegalArgumentException("Sorting term and/or order were unexpectedly null");
	}

	final HttpGet getRequest = new HttpGet(
		this.baseUrl + "/pads/?sort=" + sortingTerm.toString() + "," + sortingOrder.toString());
	getRequest.addHeader(AUTHORIZATION_HEADER, generateTokenHeaderValue());
	getRequest.setHeader("Accept", "application/json");

	CloseableHttpClient httpclient = HttpClients.createDefault();
	CloseableHttpResponse getResponse = null;
	try {
	    getResponse = httpclient.execute(getRequest);
	    final HttpEntity responseEntity = getResponse.getEntity();
	    if (responseEntity != null) {
		final ListPadsResponse listPadsResponse = objectMapper.readValue(responseEntity.getContent(),
			ListPadsResponse.class);
		if (!OK_STATUS.equals(listPadsResponse.getStatus())) {
		    throw new CoderpadException(listPadsResponse.getMessage());
		}
		return listPadsResponse;
	    } else {
		throw new CoderpadException("Empty response from Coderpad");
	    }
	} catch (Exception e) {
	    throw new CoderpadException("Error listing pads on Coderpad", e);
	} finally {
	    try {
		if (getResponse != null)
		    getResponse.close();
	    } catch (IOException e) {
		// ignore
	    }
	}
    }

    /**
     * Create a multi-valued hashmap with the attributes given in the request.
     * 
     * @param request
     * @return
     */
    private List<NameValuePair> createHttpFormForPadRequest(PadRequest request) {
	List<NameValuePair> form = new ArrayList<NameValuePair>();

	// set form fields, using defaults when appropriate
	if (request.getTitle() != null) {
	    form.add(new BasicNameValuePair("title", request.getTitle()));
	}
	if (request.getLanguage() != null) {
	    form.add(new BasicNameValuePair("language", request.getLanguage().toString()));
	}
	if (request.getContents() != null && request.getContents().length() > 0) {
	    form.add(new BasicNameValuePair("contents", request.getContents()));
	}
	if (request.isLocked()) {
	    form.add(new BasicNameValuePair("locked", Boolean.TRUE.toString()));
	}
	if (request.isPrivatePad()) {
	    form.add(new BasicNameValuePair("private", Boolean.TRUE.toString()));
	}
	if (request.isExecutionEnabled() == false) {
	    form.add(new BasicNameValuePair("execution_enabled", Boolean.FALSE.toString()));
	}
	return form;
    }

    private String generateTokenHeaderValue() {
	return "Token token=\"" + this.authenticationToken + "\"";
    }
}
