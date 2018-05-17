package eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian;

import eu.europa.ec.fisheries.schema.rules.customrule.v1.CustomRuleType;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(Arquillian.class)
public class CustomRulesRestResourceTest extends TransactionalTests {

    @Test
    public void CreateCustomRuleTest(){
        CustomRuleType customRuleType = new CustomRuleType();
        customRuleType.setName("TestCustomRule");
        customRuleType.setGuid("TestCustomRule");
        customRuleType.setDescription("TestCustomRule");
        //customRuleType.getTimeIntervals().add("CustomRuleIntervalType");

    }
}
