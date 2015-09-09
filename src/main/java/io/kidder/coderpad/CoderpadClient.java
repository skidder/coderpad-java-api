/**
 * 
 */
package io.kidder.coderpad;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.MediaType;

import org.glassfish.jersey.client.ClientConfig;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.jaxrs.json.JacksonJaxbJsonProvider;
import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;

import io.kidder.coderpad.response.ListPadsResponse;
import io.kidder.coderpad.response.PadResponse;

/**
 * @author Scott Kidder
 *
 */
public class CoderpadClient {

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
     */
    public PadResponse getPad(String padId) {
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	return client.target(this.baseUrl).path("/pads/" + padId).queryParam("_key", this.authenticationToken)
		.request(MediaType.APPLICATION_JSON_TYPE).header("Authorization", generateTokenHeaderValue())
		.get(PadResponse.class);
    }

    /**
     * List pads using the default sort order (created_at,desc).
     * 
     * @return
     */
    public ListPadsResponse listPads() {
	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	return client.target(this.baseUrl).path("/pads/").queryParam("_key", this.authenticationToken)
		.request(MediaType.APPLICATION_JSON_TYPE).header("Authorization", generateTokenHeaderValue())
		.get(ListPadsResponse.class);
    }

    /**
     * List pads using the specified sorting term and method.
     * 
     * @param sortingTerm
     * @param sortingOrder
     * @return
     */
    public ListPadsResponse listPads(SortingTerm sortingTerm, SortingOrder sortingOrder) {
	final String sortingTermString;
	switch (sortingTerm) {
	case CREATED_AT:
	    sortingTermString = "created_at";
	    break;
	case UPDATED_AT:
	    sortingTermString = "updated_at";
	    break;
	default:
	    throw new IllegalArgumentException("Unrecognized sorting term in listPads argument");
	}

	final String sortingOrderString;
	switch (sortingOrder) {
	case ASCENDING:
	    sortingOrderString = "asc";
	    break;
	case DESCENDING:
	    sortingOrderString = "desc";
	    break;
	default:
	    throw new IllegalArgumentException("Unrecognized sorting order in listPads argument");
	}

	final Client client = ClientBuilder.newClient(new ClientConfig(jacksonJsonProvider));
	return client.target(this.baseUrl).path("/pads/")
		.queryParam("sort", sortingTermString + "," + sortingOrderString)
		.queryParam("_key", this.authenticationToken).request(MediaType.APPLICATION_JSON_TYPE)
		.header("Authorization", generateTokenHeaderValue()).get(ListPadsResponse.class);
    }

    private String generateTokenHeaderValue() {
	return "Token token=\"" + this.authenticationToken + "\"";
    }
}
