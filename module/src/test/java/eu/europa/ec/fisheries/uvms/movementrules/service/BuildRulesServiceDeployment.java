package eu.europa.ec.fisheries.uvms.movementrules.service;

import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollType;
import eu.europa.ec.fisheries.uvms.commons.date.*;
import eu.europa.ec.fisheries.uvms.commons.rest.filter.MDCFilter;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.dto.SimpleCreatePoll;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

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
        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.rest");

        testWar.deleteClass(UnionVMSRestMock.class);
        testWar.deleteClass(SpatialModuleMock.class);
        testWar.addClass(MDCFilter.class);

        testWar.addAsResource("persistence-integration.xml", "META-INF/persistence.xml");

        testWar.addAsResource("beans.xml", "META-INF/beans.xml");

        testWar.delete("/WEB-INF/web.xml");
        testWar.addAsWebInfResource("mock-web.xml", "web.xml");

        return testWar;
    }

    @Deployment(name = "uvms", order = 1)
    public static Archive<?> createAssetRestMock() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "unionvms.war");

        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.spatialSwe:spatial-model",
                        "eu.europa.ec.fisheries.uvms.user:user-model",
                        "eu.europa.ec.fisheries.uvms:usm4uvms",
                        "eu.europa.ec.fisheries.uvms.commons:uvms-commons-date")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);

        testWar.addClass(UnionVMSRestMock.class);
        testWar.addClass(SpatialModuleMock.class);
        testWar.addClass(UserRestMock.class);

        testWar.addClass(ExchangeModuleRestMock.class);
        testWar.addClass(EmailType.class);

        testWar.addClass(AssetModuleMock.class);
        testWar.addClass(SimpleCreatePoll.class);
        testWar.addClass(PollType.class);

        testWar.addClass(AreaTransitionsDTO.class);
        testWar.addPackage("eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1");

        return testWar;
    }
}
