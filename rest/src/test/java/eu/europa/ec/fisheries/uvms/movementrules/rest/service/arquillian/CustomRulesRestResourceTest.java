package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import java.util.Date;
import java.util.List;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesUtil;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.type.CollectionType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetCustomRuleListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;

//@RunAsClient
@RunWith(Arquillian.class)
public class CustomRulesRestResourceTest extends TransactionalTests {

    @Test
    public void createAndDeleteCustomRuleTest() throws Exception{
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        CustomRuleType returnCrt = deserializeResponseDto(response, CustomRuleType.class);

        Assert.assertEquals(customRule.getName(), returnCrt.getName());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);

        Assert.assertEquals(200, getReturnCode(deleteResponse));

    }
    @Test
    public void createAndDeleteTwoCustomRules() throws Exception{
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        CustomRuleType returnCrt = deserializeResponseDto(response, CustomRuleType.class);
        Assert.assertNotNull(returnCrt.getGuid());

        CustomRuleType customRule2 = RulesTestHelper.getCompleteNewCustomRule();

        String response2 = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule2), String.class);
        Assert.assertEquals(200, getReturnCode(response2));
        CustomRuleType returnCrt2 = deserializeResponseDto(response2, CustomRuleType.class);
        Assert.assertNotNull(returnCrt2.getGuid());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        Assert.assertEquals(200, getReturnCode(deleteResponse));

        deleteResponse = getWebTarget().path("/customrules/" + returnCrt2.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        Assert.assertEquals(200, getReturnCode(deleteResponse));
    }

    @Test
    public void createInvalidCustomRule() throws Exception{
        CustomRuleType customRule = new CustomRuleType();
        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));


        customRule.setName("");
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));


        customRule.setName("Actual rule name");
        CustomRuleIntervalType customRuleIntervalType = new CustomRuleIntervalType();
        customRuleIntervalType.setEnd(RulesUtil.dateToString(new Date()));
        customRule.getTimeIntervals().add(customRuleIntervalType);
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(500, getReturnCode(response)); //500 since this one is a spontaneous null pointer....


        customRuleIntervalType.setStart(RulesUtil.dateToString(new Date(System.currentTimeMillis() + 1000L)));
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));


        customRule = RulesTestHelper.getCompleteNewCustomRule();
        CustomRuleSegmentType crst = customRule.getDefinitions().get(0);
        crst.setLogicBoolOperator(LogicOperatorType.NONE);
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));


        crst.setLogicBoolOperator(LogicOperatorType.AND);
        crst = customRule.getDefinitions().get(1);
        crst.setLogicBoolOperator(LogicOperatorType.AND);
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));

        crst.setLogicBoolOperator(LogicOperatorType.NONE);
        crst.setEndOperator("");
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));

        crst.setStartOperator("");
        crst.setEndOperator(")");
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(511, getReturnCode(response));

        crst.setStartOperator("(");
        customRule.setAvailability(AvailabilityType.GLOBAL);
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(403, getReturnCode(response));  //403 = forbidden

        customRule.setAvailability(AvailabilityType.PRIVATE);
        crst.setStartOperator("hi");
        crst.setEndOperator("by");
        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class); //this really should not work.......
        Assert.assertEquals(511, getReturnCode(response));
    }

    @Test
    public void createChangeSubscriptionAndDeleteCustomRule() throws Exception {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        CustomRuleType returnCrt = deserializeResponseDto(response, CustomRuleType.class);

        UpdateSubscriptionType updateSubscriptionType = new UpdateSubscriptionType();
        SubscriptionType subscriptionType = returnCrt.getSubscriptions().get(0);
        updateSubscriptionType.setSubscription(subscriptionType);

        updateSubscriptionType.setRuleGuid(returnCrt.getGuid());

        updateSubscriptionType.setOperation(SubscritionOperationType.REMOVE);

        response = getWebTarget().path("/customrules/subscription").request(MediaType.APPLICATION_JSON).post(Entity.json(updateSubscriptionType), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        returnCrt = deserializeResponseDto(response, CustomRuleType.class);
        Assert.assertTrue(returnCrt.getSubscriptions().isEmpty());

        getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);

    }

    @Test
    public void changeInvalidSubscription() throws Exception {
        String response = getWebTarget().path("/customrules/subscription").request(MediaType.APPLICATION_JSON).post(Entity.json(new UpdateSubscriptionType()), String.class);
        Assert.assertEquals(500, getReturnCode(response));
    }

    @Test
    public void createUpdateAndDeleteCustomRule() throws Exception{
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        CustomRuleType returnCrt = deserializeResponseDto(response, CustomRuleType.class);

        customRule.setName("New name");
        customRule.setGuid(returnCrt.getGuid());


        response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).put(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        returnCrt = deserializeResponseDto(response, CustomRuleType.class);
        Assert.assertEquals(customRule.getName(), returnCrt.getName());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        Assert.assertEquals(200, getReturnCode(deleteResponse));
    }

    @Test
    public void updateInvalidCustomRule() throws Exception {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).put(Entity.json(customRule), String.class);
        Assert.assertEquals(500, getReturnCode(response));
    }

    @Test
    public void createFindByGuidAndDeleteCustomRule() throws Exception{
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        CustomRuleType returnCrt = deserializeResponseDto(response, CustomRuleType.class);

        customRule.setGuid(returnCrt.getGuid());


        response = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).get(String.class);
        Assert.assertEquals(200, getReturnCode(response));
        returnCrt = deserializeResponseDto(response, CustomRuleType.class);

        Assert.assertEquals(customRule.getGuid(), returnCrt.getGuid());
        Assert.assertEquals(customRule.getName(), returnCrt.getName());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        Assert.assertEquals(200, getReturnCode(deleteResponse));
    }

    @Test
    public void findCustomRuleByInvalidGuid() throws Exception {
        String response = getWebTarget().path("/customrules/" + "invalidCustomRule").request(MediaType.APPLICATION_JSON).get(String.class);
        Assert.assertEquals(500, getReturnCode(response));
    }

    @Test
    public void createFindByUserNameAndDelete() throws Exception{
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();
        customRule.setDescription("Test description");
        customRule.setUpdatedBy("vms_admin_com_createFindByUserNameAndDelete");

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));


        response = getWebTarget().path("/customrules/listAll/" + customRule.getUpdatedBy()).request(MediaType.APPLICATION_JSON).get(String.class);
        Assert.assertEquals(200, getReturnCode(response));

        final ObjectNode data = OBJECT_MAPPER.readValue(response, ObjectNode.class);
        String valueAsString = OBJECT_MAPPER.writeValueAsString(data.get("data"));
        CollectionType ct = OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, CustomRuleType.class);
        List<CustomRuleType> dataValue = OBJECT_MAPPER.readValue(valueAsString, ct);

        CustomRuleType returnCrt = dataValue.get(0);
        Assert.assertEquals(customRule.getName(), returnCrt.getName());
        Assert.assertEquals(customRule.getDescription(), returnCrt.getDescription());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        Assert.assertEquals(200, getReturnCode(deleteResponse));
    }

    @Test
    public void createFindByQueryAndDelete() throws Exception {
        CustomRuleType customRule = RulesTestHelper.getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));

        CustomRuleQuery customRuleQuery = new CustomRuleQuery();
        ListPagination lp = new ListPagination();
        lp.setListSize(10);
        lp.setPage(1); //this value can not be 0 or lower...... ;(
        customRuleQuery.setPagination(lp);
        CustomRuleListCriteria crlc = new CustomRuleListCriteria();
        crlc.setKey(CustomRuleSearchKey.NAME);
        crlc.setValue(customRule.getName());
        customRuleQuery.getCustomRuleSearchCriteria().add(crlc);
        customRuleQuery.setDynamic(true);

        response = getWebTarget().path("/customrules/listByQuery").request(MediaType.APPLICATION_JSON).post(Entity.json(customRuleQuery), String.class);
        Assert.assertEquals(200, getReturnCode(response));

        GetCustomRuleListByQueryResponse returnResult = deserializeResponseDto(response, GetCustomRuleListByQueryResponse.class);

        CustomRuleType returnCrt = returnResult.getCustomRules().get(0);
        Assert.assertEquals(customRule.getName(), returnCrt.getName());
        Assert.assertEquals(customRule.getActions().get(0).getValue(), returnCrt.getActions().get(0).getValue());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);
        Assert.assertEquals(200, getReturnCode(deleteResponse));

    }

    @Test
    public void findCustomRuleByInvalidQuery() throws Exception {
        String response = getWebTarget().path("/customrules/listByQuery").request(MediaType.APPLICATION_JSON).post(Entity.json(new CustomRuleQuery()), String.class);
        Assert.assertEquals(500, getReturnCode(response));
    }

    private static <T> T deserializeResponseDto(String responseDto, Class<T> clazz) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode node = objectMapper.readValue(responseDto, ObjectNode.class);
        JsonNode jsonNode = node.get("data");
        return objectMapper.readValue(objectMapper.writeValueAsString(jsonNode), clazz);
    }

    private int getReturnCode(String responsDto) throws Exception{
         return OBJECT_MAPPER.readValue(responsDto, ObjectNode.class).get("code").asInt();
    }

}
