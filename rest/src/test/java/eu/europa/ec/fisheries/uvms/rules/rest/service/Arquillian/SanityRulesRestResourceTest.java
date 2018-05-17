package eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.type.CollectionType;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SanityRuleType;

import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ws.rs.core.MediaType;
import java.util.List;

@RunWith(Arquillian.class)
public class SanityRulesRestResourceTest extends TransactionalTests {

    @Test
    public void worldsBestTestTest(){
        Assert.assertTrue(true);
    }

    @Test
    @RunAsClient
    public void getSanityRulesTest() throws Exception{
        String response = getWebTarget().path("/sanityrules/listAll").request(MediaType.APPLICATION_JSON).get(String.class);


        final ObjectNode data = OBJECT_MAPPER.readValue(response, ObjectNode.class);


        Assert.assertEquals(200, data.get("code").asInt());

        //it feels wrong to use read value twice but I have yet to get it to work any other way.
        String valueAsString = OBJECT_MAPPER.writeValueAsString(data.get("data"));
        CollectionType ct = OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, SanityRuleType.class);
        List<SanityRuleType> dataValue = OBJECT_MAPPER.readValue(valueAsString, ct);
        //final MapType type = OBJECT_MAPPER.getTypeFactory().construct(Map.class, String.class, Object.class);
        //final ObjectNode data2 = OBJECT_MAPPER.treeToValue(jsonNode, new TypeReference<List<SanityRuleType >>(){});

        Assert.assertEquals(12, dataValue.size());


    }



}
