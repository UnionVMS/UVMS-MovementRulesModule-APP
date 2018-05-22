package eu.europa.ec.fisheries.uvms.rules.service.bean;

import javax.inject.Inject;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.uvms.rules.service.ValidationService;

@RunWith(Arquillian.class)
public class ValidationServiceBeanTest {

    @Inject
    ValidationService validationService;
    
    @Test
    public void test() {
        
    }
}
