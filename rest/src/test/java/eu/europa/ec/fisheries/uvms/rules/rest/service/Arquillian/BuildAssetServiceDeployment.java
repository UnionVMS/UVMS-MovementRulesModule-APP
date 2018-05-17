package eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.jaxrs.json.JacksonJaxbJsonProvider;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import java.io.File;
import java.util.Arrays;

@ArquillianSuiteDeployment
public abstract class BuildAssetServiceDeployment {

    @Deployment(name = "normal", order = 1)
    public static Archive<?> createDeployment() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "test.war");

//        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
//                .importRuntimeAndTestDependencies().resolve().withTransitivity().asFile();
//        testWar.addAsLibraries(files);

        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml").importRuntimeAndTestDependencies().resolve().withTransitivity().asFile();
                /*.resolve("eu.europa.ec.fisheries.uvms.rules:rules-dbaccess-domain",
                         "eu.europa.ec.fisheries.uvms.rules:rules-message",
                         //"eu.europa.ec.fisheries.uvms.asset:asset-message-mock",
                         "eu.europa.ec.fisheries.uvms.model:rules-model",
                         "eu.europa.ec.fisheries.uvms:uvms-config",
                         "eu.europa.ec.fisheries.uvms.config:config-model:4.0.0")
                .withoutTransitivity().asFile();*/
        //Arrays.stream(files).forEach(System.out::println);
        testWar.addAsLibraries(files);
        
        //testWar.addPackages(true, "eu.europa.fisheries.uvms.rules.rest");
        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.rules.rest");

        testWar.delete("/WEB-INF/web.xml");
        testWar.addAsWebInfResource("mock-web.xml", "web.xml");

        return testWar;
    }

    protected WebTarget getWebTarget() {


        ObjectMapper objectMapper = new ObjectMapper();
        Client client = ClientBuilder.newClient();
        client.register(new JacksonJaxbJsonProvider(objectMapper, JacksonJaxbJsonProvider.DEFAULT_ANNOTATIONS));
        return client.target("http://localhost:28080/test/rest");
    }
}