package io.kidder.coderpad.response;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class ListPadsResponseTest {

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void test() throws IOException, JsonMappingException, IOException {
	ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally
	ListPadsResponse response = mapper.readValue(new File("src/test/resources/list_pads_response.json"), ListPadsResponse.class);
	assertNotNull(response);
	assertEquals("OK", response.getStatus());
	assertNotNull(response.getNextPageUrl());
	assertEquals(420, response.getTotal());
	
	final PadResponse pad = response.getPads().get(0);
	assertEquals("5.times do\n  puts 'Hello, World!'\nend\n", pad.getContents());
	assertNotNull(pad.getCreatedAt());
	assertNotNull(pad.getUpdatedAt());
	assertEquals(5, pad.getEvents().size());
    }

}
