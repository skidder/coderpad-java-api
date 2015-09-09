package io.kidder.coderpad.response;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

public class PadResponseTest {

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testUnmarshal() throws IOException {
	ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally
	PadResponse response = mapper.readValue(new File("src/test/resources/get_pad_response.json"), PadResponse.class);
	assertNotNull(response);

	assertEquals("OK", response.getStatus());
	assertNull(response.getMessage());
	assertEquals("5.times do\n  puts 'Hello, World!'\nend\n", response.getContents());
	assertNotNull(response.getCreatedAt());
	assertNotNull(response.getUpdatedAt());
	assertEquals(5, response.getEvents().size());
    }

}
