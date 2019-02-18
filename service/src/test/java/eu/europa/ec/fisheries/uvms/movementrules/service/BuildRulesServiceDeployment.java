package eu.europa.ec.fisheries.uvms.movementrules.service;

import java.io.File;

import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollBaseType;
import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollRequestType;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ArquillianSuiteDeployment
public abstract class BuildRulesServiceDeployment {

    final static Logger LOG = LoggerFactory.getLogger(BuildRulesServiceDeployment.class);

    @Deployment(name = "normal", order = 2)
    public static Archive<?> createDeployment() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "test.war");

        File[] files = Maven.resolver().loadPomFromFile("pom.xml").importRuntimeAndTestDependencies().resolve()
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addAsResource(new File("src/main/resources/templates/CustomRulesTemplate.drt"), "/templates/CustomRulesTemplate.drt");

        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.service");

        testWar.addAsResource("persistence-integration.xml", "META-INF/persistence.xml");

        testWar.addAsResource("beans.xml", "META-INF/beans.xml");

        return testWar;
    }

    @Deployment(name = "uvms", order = 1)
    public static Archive<?> createAssetRestMock() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "unionvms.war");

        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.spatial:spatial-model")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addClass(UnionVMSRestMock.class);
        testWar.addClass(SpatialModuleMock.class);
        testWar.addClass(AreaTransitionsDTO.class);
        testWar.addClass(AssetModuleMock.class);
        testWar.addClass(PollBaseType.class);
        testWar.addClass(PollRequestType.class);

        return testWar;
    }
}
