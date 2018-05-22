package eu.europa.Arquillian;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.type.CollectionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.*;
import eu.europa.ec.fisheries.uvms.rules.service.RulesService;
import eu.europa.ec.fisheries.uvms.rules.service.exception.InputArgumentException;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ejb.EJB;
import javax.ejb.EJBTransactionRolledbackException;
import javax.persistence.NoResultException;
import java.nio.file.AccessDeniedException;


@RunWith(Arquillian.class)
public class RulesServiceTest extends TransactionalTests {


    @EJB
    RulesService rulesService;

    @Test
    public void worldsBestTestTest(){
        Assert.assertTrue(true);
    }


    @Test
    public void createCustomRuleTest() throws Exception{
        CustomRuleType input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL);

        CustomRuleType output = null;
        try {
            output = rulesService.createCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }

        input.setAvailability((AvailabilityType.PRIVATE));
        output = rulesService.createCustomRule(input, "test", "test");
        Assert.assertNotNull(output.getGuid());
    }

    @Test
    public void getCustomRuleByDummyGuidTest() throws Exception{   //a get with proper input exists among the rest tests
        try {
            rulesService.getCustomRuleByGuid("dummyGuid");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    public void updateDummyCustomRuleTest() throws Exception{      //an update with proper input/execution exists among the rest tests
        CustomRuleType input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL);

        //not enough rights
        try {
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }
        input.setAvailability(AvailabilityType.PRIVATE);

        //no guid
        try{
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        //null as input, funnily enough they die at different places......
        try{
            rulesService.updateCustomRule(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(null, "test", "test");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        input.setGuid("dummyGuid");

        //no such rule to update
        try{
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

    }

    @Test
    public void deleteCustomRuleWithDummyGuidTest() throws Exception {          //a test with proper exectuion exists among the rest tests
        try {
            rulesService.deleteCustomRule(null, "testUser", "testFeature", "testApp");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try {
            rulesService.deleteCustomRule("dummyGuid", "testUser", "testFeature", "testApp");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    public void updateSubscriptionNegativeTests() throws Exception{
        try{
            rulesService.updateSubscription(null, "testUser");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }


        UpdateSubscriptionType input = new UpdateSubscriptionType();
        SubscriptionType subscriptionType = new SubscriptionType();
        input.setSubscription(subscriptionType);

        //incomplete subscriptionType
        try{
            rulesService.updateSubscription(input, "testUser");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        subscriptionType.setType(SubscriptionTypeType.TICKET);
        try{
            rulesService.updateSubscription(input, "testUser");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        subscriptionType.setOwner("tester");

        try{
            rulesService.updateSubscription(input, "testUser"); //no rule Guid
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        input.setRuleGuid("dummyGuid");

        try{
            rulesService.updateSubscription(input, "testUser"); //non-existant rule guid
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

    }

    @Test
    public void updateSubscriptionTest() throws Exception{
        UpdateSubscriptionType input = new UpdateSubscriptionType();
        SubscriptionType subscriptionType = new SubscriptionType();
        input.setSubscription(subscriptionType);
        subscriptionType.setType(SubscriptionTypeType.TICKET);
        subscriptionType.setOwner("tester");

        //create a custom rule and do some actual updates.....
        CustomRuleType newRule = getCompleteNewCustomRule();
        newRule = rulesService.createCustomRule(newRule, "test", "test");
        Assert.assertNotNull(newRule.getGuid());

        input.setRuleGuid(newRule.getGuid());

        input.setOperation(SubscritionOperationType.ADD);
        CustomRuleType output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(2, output.getSubscriptions().size());

        input.setOperation((SubscritionOperationType.REMOVE));
        output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(1, output.getSubscriptions().size());
        Assert.assertEquals("vms_admin_com", output.getSubscriptions().get(0).getOwner());
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
