package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import java.io.File;
import java.util.Arrays;
import javax.ejb.EJB;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import eu.europa.ec.fisheries.uvms.commons.rest.filter.MDCFilter;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.dto.AreaTransitionsDTO;
import eu.europa.ec.fisheries.uvms.movementrules.service.JsonBConfigurator;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.SpatialModuleMock;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.UnionVMSRestMock;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.UserRestMock;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import eu.europa.ec.mare.usm.jwt.JwtTokenHandler;

@ArquillianSuiteDeployment
public abstract class BuildRulesRestDeployment {

    @EJB
    private JwtTokenHandler tokenHandler;

    private String token;
    
    @Deployment(name = "normal", order = 2)
    public static Archive<?> createDeployment() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "test.war");
        
        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.movement-rules:movement-rules-service",
                        "eu.europa.ec.fisheries.uvms:usm4uvms")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.rest");

        testWar.deleteClass(UnionVMSRestMock.class);
        testWar.deleteClass(SpatialModuleMock.class);
        testWar.addClass(MDCFilter.class);
        
        testWar.delete("/WEB-INF/web.xml");
        testWar.addAsWebInfResource("mock-web.xml", "web.xml");

        return testWar;
    }

    protected WebTarget getWebTarget() {
        return ClientBuilder.newClient().register(JsonBConfigurator.class).target("http://localhost:8080/test/rest");
    }


    @Deployment(name = "uvms", order = 1)
    public static Archive<?> createAssetRestMock() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "unionvms.war");
        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.spatialSwe:spatial-model",
                        "eu.europa.ec.fisheries.uvms.user:user-model",
                        "eu.europa.ec.fisheries.uvms:usm4uvms")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addClass(UnionVMSRestMock.class);
        testWar.addClass(SpatialModuleMock.class);
        testWar.addClass(UserRestMock.class);
        testWar.addClass(AreaTransitionsDTO.class);
        
        return testWar;
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
