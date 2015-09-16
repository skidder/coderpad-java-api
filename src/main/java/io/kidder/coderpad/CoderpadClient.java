/**
 * 
 */
package io.kidder.coderpad;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedHashMap;

import org.glassfish.jersey.client.ClientConfig;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.jaxrs.json.JacksonJaxbJsonProvider;
import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;

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
    private JacksonJsonProvider jacksonJsonProvider;

    public CoderpadClient(String authenticationToken) {
	this(authenticationToken, CODERPAD_BASE_URL);
    }

    public CoderpadClient(String authenticationToken, String baseUrl) {
	this.authenticationToken = authenticationToken;
	this.baseUrl = baseUrl;
	jacksonJsonProvider = new JacksonJaxbJsonProvider().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES,
		false);
    }

    /**
     * Get a pad resource by its ID
     * 
     * @param padId
     * @return
     * @throws CoderpadException
     */
    public PadResponse getPad(String padId) throws CoderpadException {
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	PadResponse response = client.target(this.baseUrl).path("/pads/" + padId)
		.request(MediaType.APPLICATION_JSON_TYPE).header(AUTHORIZATION_HEADER, generateTokenHeaderValue())
		.get(PadResponse.class);
	if (!OK_STATUS.equals(response.getStatus())) {
	    throw new CoderpadException(response.getMessage());
	}
	return response;
    }

    /**
     * List pads using the default sort order (created_at,desc).
     * 
     * @return
     * @throws CoderpadException
     */
    public ListPadsResponse listPads() throws CoderpadException {
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	ListPadsResponse response = client.target(this.baseUrl).path("/pads/").request(MediaType.APPLICATION_JSON_TYPE)
		.header(AUTHORIZATION_HEADER, generateTokenHeaderValue()).get(ListPadsResponse.class);
	if (!OK_STATUS.equals(response.getStatus())) {
	    throw new CoderpadException(response.getMessage());
	}
	return response;
    }

    /**
     * Create a new pad with the attributes specified in the request.
     * 
     * @param request
     * @return
     * @throws CoderpadException
     */
    public PadResponse createPad(PadRequest request) throws CoderpadException {
	final MultivaluedHashMap<String, String> form = createMultiValuedMapForPadRequest(request);
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	PadResponse response = client.target(this.baseUrl).path("/pads/").request()
		.header(AUTHORIZATION_HEADER, generateTokenHeaderValue())
		.post(Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE), PadResponse.class);
	if (!OK_STATUS.equals(response.getStatus())) {
	    throw new CoderpadException(response.getMessage());
	}
	return response;
    }

    /**
     * Update an existing pad.
     * 
     * @param id
     * @param request
     * @throws CoderpadException
     */
    public void updatePad(String id, PadRequest request) throws CoderpadException {
	final MultivaluedHashMap<String, String> form = createMultiValuedMapForPadRequest(request);
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	BaseResponse response = client.target(this.baseUrl).path("/pads/" + id).request()
		.header(AUTHORIZATION_HEADER, generateTokenHeaderValue())
		.put(Entity.entity(form, MediaType.APPLICATION_FORM_URLENCODED_TYPE), BaseResponse.class);
	if (!OK_STATUS.equals(response.getStatus())) {
	    throw new CoderpadException(response.getMessage());
	}
    }

    /**
     * Delete an existing pad.
     * 
     * @param id
     * @throws CoderpadException
     */
    public void deletePad(String id) throws CoderpadException {
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	BaseResponse response = client.target(this.baseUrl).path("/pads/" + id).request()
		.header(AUTHORIZATION_HEADER, generateTokenHeaderValue()).delete(BaseResponse.class);
	if (!OK_STATUS.equals(response.getStatus())) {
	    throw new CoderpadException(response.getMessage());
	}
    }

    /**
     * List pads using the specified sorting term and method.
     * 
     * @param sortingTerm
     * @param sortingOrder
     * @return
     */
    public ListPadsResponse listPads(ListPadsSortingTerm sortingTerm, ListPadsSortingOrder sortingOrder) {
	if (sortingTerm == null || sortingOrder == null) {
	    throw new IllegalArgumentException("Sorting term and/or order were unexpectedly null");
	}

	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	return client.target(this.baseUrl).path("/pads/")
		.queryParam("sort", sortingTerm.toString() + "," + sortingOrder.toString())
		.request(MediaType.APPLICATION_JSON_TYPE).header(AUTHORIZATION_HEADER, generateTokenHeaderValue())
		.get(ListPadsResponse.class);
    }

    /**
     * Create a multi-valued hashmap with the attributes given in the request.
     * 
     * @param request
     * @return
     */
    private MultivaluedHashMap<String, String> createMultiValuedMapForPadRequest(PadRequest request) {
	// set form fields, using defaults when appropriate
	MultivaluedHashMap<String, String> form = new MultivaluedHashMap<String, String>();
	if (request.getTitle() != null) {
	    form.add("title", request.getTitle());
	}
	if (request.getLanguage() != null) {
	    form.add("language", request.getLanguage().toString());
	}
	if (request.getContents() != null && request.getContents().length() > 0) {
	    form.add("contents", request.getContents());
	}
	if (request.isLocked()) {
	    form.add("locked", Boolean.TRUE.toString());
	}
	if (request.isPrivatePad()) {
	    form.add("private", Boolean.TRUE.toString());
	}
	if (request.isExecutionEnabled() == false) {
	    form.add("execution_enabled", Boolean.FALSE.toString());
	}
	return form;
    }

    private String generateTokenHeaderValue() {
	return "Token token=\"" + this.authenticationToken + "\"";
    }
}
