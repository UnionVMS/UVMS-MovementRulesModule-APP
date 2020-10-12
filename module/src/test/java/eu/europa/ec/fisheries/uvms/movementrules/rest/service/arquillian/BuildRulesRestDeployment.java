package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import java.util.Arrays;
import java.util.List;
import javax.inject.Inject;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import org.junit.AfterClass;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.uvms.commons.date.JsonBConfigurator;
import eu.europa.ec.fisheries.uvms.movementrules.service.BuildRulesServiceDeployment;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import eu.europa.ec.mare.usm.jwt.JwtTokenHandler;

public abstract class BuildRulesRestDeployment extends BuildRulesServiceDeployment {

    @Inject
    private JwtTokenHandler tokenHandler;

    private String token;
   
    protected WebTarget getWebTarget() {
        return ClientBuilder.newClient().register(JsonBConfigurator.class).target("http://localhost:8080/test/rest");
    }

    protected String getToken() {
        if (token == null) {
            token = tokenHandler.createToken("user", 
                    Arrays.asList(UnionVMSFeature.viewAlarmRules.getFeatureId(), 
                            UnionVMSFeature.manageAlarmRules.getFeatureId(),
                            UnionVMSFeature.manageAlarmsOpenTickets.getFeatureId(),
                            UnionVMSFeature.manageGlobalAlarmsRules.getFeatureId(),
                            UnionVMSFeature.viewAlarmsOpenTickets.getFeatureId()));
        }
        return token;
    }
    
    protected String getTokenExternal() {
        if (token == null) {
            token = ClientBuilder.newClient()
                    .target("http://localhost:8080/unionvms/user/rest/user/token")
                    .request(MediaType.APPLICATION_JSON)
                    .get(String.class);
        }
        return token;
    }
}
