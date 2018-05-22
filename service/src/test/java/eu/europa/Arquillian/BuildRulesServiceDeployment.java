package eu.europa.Arquillian;

import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;

import java.io.File;

@ArquillianSuiteDeployment
public abstract class BuildRulesServiceDeployment {

    @Deployment(name = "normal", order = 1)
    public static Archive<?> createDeployment() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "test.war");

        File[] files = Maven.resolver().loadPomFromFile("pom.xml").importRuntimeAndTestDependencies().resolve()
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addAsResource(new File("src/main/resources/templates/CustomRulesTemplate.drt"),"/templates/CustomRulesTemplate.drt");
        testWar.addAsResource(new File("src/main/resources/templates/SanityRulesTemplate.drt"),"/templates/SanityRulesTemplate.drt");

        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.rules.service");
        testWar.addPackages(true, "eu.europa.Arquillian");

        return testWar;
    }

}
