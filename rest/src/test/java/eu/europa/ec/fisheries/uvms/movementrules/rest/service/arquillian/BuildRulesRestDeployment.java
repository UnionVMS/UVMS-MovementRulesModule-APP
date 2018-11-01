package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import java.io.File;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;

import eu.europa.ec.fisheries.uvms.movementrules.rest.RestActivator;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.UnionVMSRestMock;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.jaxrs.json.JacksonJaxbJsonProvider;

@ArquillianSuiteDeployment
public abstract class BuildRulesRestDeployment {

    @Deployment(name = "normal", order = 2)
    public static Archive<?> createDeployment() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "test.war");

        File[] files = Maven.resolver().loadPomFromFile("pom.xml").importRuntimeAndTestDependencies().resolve()
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);
        
        testWar.addAsLibraries(Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.movement-rules:movement-rules-service")
                .withTransitivity().asFile());

        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.rest");

        testWar.delete("/WEB-INF/web.xml");
        testWar.addAsWebInfResource("mock-web.xml", "web.xml");

        return testWar;
    }

    protected WebTarget getWebTarget() {

        ObjectMapper objectMapper = new ObjectMapper();
        Client client = ClientBuilder.newClient();
        client.register(new JacksonJaxbJsonProvider(objectMapper, JacksonJaxbJsonProvider.DEFAULT_ANNOTATIONS));
        //return client.target("http://localhost:28080/test/rest");
        return client.target("http://localhost:8080/test/rest");
    }


    @Deployment(name = "uvms", order = 1)
    public static Archive<?> createAssetRestMock() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "unionvms.war");
        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.asset:asset-model",
                        "eu.europa.ec.fisheries.uvms.asset:asset-client",
                        "eu.europa.ec.fisheries.uvms.spatial:spatial-model",
                        "eu.europa.ec.fisheries.uvms.commons:uvms-commons-message")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addClass(UnionVMSRestMock.class);
        testWar.addClass(AssetMTRestMock.class);
        testWar.addClass(SpatialModuleMock.class);
        

        return testWar;
    }


}
