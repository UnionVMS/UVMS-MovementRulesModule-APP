package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@RunWith(Arquillian.class)
public class ConfigResourcesTest extends TransactionalTests {

    @Test
    @OperateOnDeployment("normal")
    public void getConfigTest() throws Exception{

        String response = getWebTarget()
                .path("config/")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        Map map = deserializeResponseDto(response, HashMap.class);
        Assert.assertEquals(6, map.keySet().size());

        for(Object o : map.keySet()){
            Assert.assertNotNull(map.get(o));
        }

        Map<String, HashMap<String, ArrayList<String>>> crit = (Map<String, HashMap<String, ArrayList<String>>>)map.get("CRITERIA");

        Assert.assertEquals(7, crit.keySet().size());
        for(String s : crit.keySet()){
            Assert.assertNotNull(crit.get(s));
        }
    }


    @Test
    @OperateOnDeployment("normal")
    public void getTicketStatusesTest() throws Exception{
        String response = getWebTarget()
                .path("config/ticketstatus")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        Object[] o = deserializeResponseDto(response, Object[].class);
        Assert.assertEquals(4, o.length);
    }


    private static <T> T deserializeResponseDto(String responseDto, Class<T> clazz) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode node = objectMapper.readValue(responseDto, ObjectNode.class);
        JsonNode jsonNode = node.get("data");
        return objectMapper.readValue(objectMapper.writeValueAsString(jsonNode), clazz);
    }
}
