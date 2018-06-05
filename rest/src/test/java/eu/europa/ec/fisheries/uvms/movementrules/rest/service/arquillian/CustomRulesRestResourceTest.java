package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import java.util.List;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
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
        CustomRuleType customRule = getCompleteNewCustomRule();

        String response = getWebTarget().path("/customrules").request(MediaType.APPLICATION_JSON).post(Entity.json(customRule), String.class);
        Assert.assertEquals(200, getReturnCode(response));
        CustomRuleType returnCrt = deserializeResponseDto(response, CustomRuleType.class);

        Assert.assertEquals(customRule.getName(), returnCrt.getName());

        String deleteResponse = getWebTarget().path("/customrules/" + returnCrt.getGuid()).request(MediaType.APPLICATION_JSON).delete(String.class);

        Assert.assertEquals(200, getReturnCode(deleteResponse));

    }

    @Test
    public void createChangeSubscriptionAndDeleteCustomRule() throws Exception {
        CustomRuleType customRule = getCompleteNewCustomRule();

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
    public void createUpdateAndDeleteCustomRule() throws Exception{
        CustomRuleType customRule = getCompleteNewCustomRule();

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
    public void createFindByGuidAndDeleteCustomRule() throws Exception{
        CustomRuleType customRule = getCompleteNewCustomRule();

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
    public void createFindByUserNameAndDelete() throws Exception{
        CustomRuleType customRule = getCompleteNewCustomRule();
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
        CustomRuleType customRule = getCompleteNewCustomRule();

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


    private static <T> T deserializeResponseDto(String responseDto, Class<T> clazz) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode node = objectMapper.readValue(responseDto, ObjectNode.class);
        JsonNode jsonNode = node.get("data");
        return objectMapper.readValue(objectMapper.writeValueAsString(jsonNode), clazz);
    }

    private int getReturnCode(String responsDto) throws Exception{
         return OBJECT_MAPPER.readValue(responsDto, ObjectNode.class).get("code").asInt();
    }

    private CustomRuleType getCompleteNewCustomRule(){
        CustomRuleType customRule = new CustomRuleType();

        customRule.setName("Flag SWE && area DNK => Send to DNK" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);

        // If flagstate = SWE
        CustomRuleSegmentType flagStateRule = new CustomRuleSegmentType();
        flagStateRule.setStartOperator("(");
        flagStateRule.setCriteria(CriteriaType.ASSET);
        flagStateRule.setSubCriteria(SubCriteriaType.FLAG_STATE);
        flagStateRule.setCondition(ConditionType.EQ);
        flagStateRule.setValue("SWE");
        flagStateRule.setEndOperator(")");
        flagStateRule.setLogicBoolOperator(LogicOperatorType.AND);
        flagStateRule.setOrder("0");
        customRule.getDefinitions().add(flagStateRule);

        // and area = DNK
        CustomRuleSegmentType areaRule = new CustomRuleSegmentType();
        areaRule.setStartOperator("(");
        areaRule.setCriteria(CriteriaType.AREA);
        areaRule.setSubCriteria(SubCriteriaType.AREA_CODE);
        areaRule.setCondition(ConditionType.EQ);
        areaRule.setValue("DNK");
        areaRule.setEndOperator(")");
        areaRule.setLogicBoolOperator(LogicOperatorType.NONE);
        areaRule.setOrder("1");
        customRule.getDefinitions().add(areaRule);

        // then send to FLUX DNK
        CustomRuleActionType action = new CustomRuleActionType();
        action.setAction(ActionType.SEND_TO_FLUX);
        action.setValue("FLUX DNK");
        action.setOrder("0");

        customRule.getActions().add(action);

        return customRule;
    }
}
