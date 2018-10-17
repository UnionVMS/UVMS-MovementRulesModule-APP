package eu.europa.ec.fisheries.uvms.movementrules.service;

import eu.europa.ec.fisheries.uvms.movementrules.service.config.MovementRulesConfigHelper;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;

import java.io.File;

@ArquillianSuiteDeployment
public abstract class BuildRulesServiceDeployment {

    @Deployment(name = "normal", order = 2)
    public static Archive<?> createDeployment() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "test.war");

        File[] files = Maven.resolver().loadPomFromFile("pom.xml").importRuntimeAndTestDependencies().resolve()
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addAsResource(new File("src/main/resources/templates/CustomRulesTemplate.drt"),"/templates/CustomRulesTemplate.drt");
        testWar.addAsResource(new File("src/main/resources/templates/SanityRulesTemplate.drt"),"/templates/SanityRulesTemplate.drt");

        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.service");
        
        testWar.addAsResource("persistence-integration.xml", "META-INF/persistence.xml");

        return testWar;
    }

    @Deployment(name = "uvms", order = 1)
    public static Archive<?> createSpatialMock() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "UnionVMS.war");

        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .importRuntimeAndTestDependencies()
                .resolve("eu.europa.ec.fisheries.uvms.asset:asset-model","eu.europa.ec.fisheries.uvms.asset:asset-client")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addAsResource(new File("src/main/resources/templates/CustomRulesTemplate.drt"),"/templates/CustomRulesTemplate.drt");
        testWar.addAsResource(new File("src/main/resources/templates/SanityRulesTemplate.drt"),"/templates/SanityRulesTemplate.drt");

        testWar.addClass(UnionVMSRestMock.class);
        testWar.addClass(AssetMTRestMock.class);

        testWar.addAsResource("persistence-integration.xml", "META-INF/persistence.xml");
        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.service");


        return testWar;
    }

}
