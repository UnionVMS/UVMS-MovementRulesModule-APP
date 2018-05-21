package eu.europa.Arquillian;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.type.CollectionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SanityRuleType;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

//TODO: RENAME
@RunWith(Arquillian.class)
public class SanityRulesRestResourceTest extends TransactionalTests {

    @Test
    public void worldsBestTestTest(){
        Assert.assertTrue(true);
    }

}
