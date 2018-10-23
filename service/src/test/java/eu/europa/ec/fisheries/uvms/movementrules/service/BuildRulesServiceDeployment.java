package eu.europa.ec.fisheries.uvms.movementrules.service;

import eu.europa.ec.fisheries.uvms.movementrules.service.config.MovementRulesConfigHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.bean.RulesEventMessageConsumerBean;
import org.eu.ingwar.tools.arquillian.extension.suite.annotations.ArquillianSuiteDeployment;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ArchivePath;
import org.jboss.shrinkwrap.api.Node;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.ShrinkWrap.*;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jboss.shrinkwrap.api.formatter.Formatter;

import java.io.*;
import java.util.*;

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
        testWar.addAsResource(new File("src/main/resources/templates/SanityRulesTemplate.drt"), "/templates/SanityRulesTemplate.drt");

        testWar.addPackages(true, "eu.europa.ec.fisheries.uvms.movementrules.service");

        testWar.addAsResource("persistence-integration.xml", "META-INF/persistence.xml");

        // dumpContent(testWar, "c:\\temp\\normal.txt");
        return testWar;
    }

    @Deployment(name = "uvms", order = 1)
    public static Archive<?> createAssetRestMock() {

        WebArchive testWar = ShrinkWrap.create(WebArchive.class, "unionvms.war");

        File[] files = Maven.configureResolver().loadPomFromFile("pom.xml")
                .resolve("eu.europa.ec.fisheries.uvms.asset:asset-model",
                        "eu.europa.ec.fisheries.uvms.asset:asset-client",
                        "eu.europa.ec.fisheries.uvms.commons:uvms-commons-message")
                .withTransitivity().asFile();
        testWar.addAsLibraries(files);


        testWar.addClass(UnionVMSRestMock.class);
        testWar.addClass(AssetMTRestMock.class);

       // dumpContent(testWar, "c:\\temp\\unionvms.txt");


        return testWar;
    }


    private static void dumpContent(WebArchive archive, String outputFileName) {

        SortedMap<String, List<String>> data = new TreeMap<>();


        try {
            OutputStream os = new FileOutputStream(outputFileName);

            archive.writeTo(os, new Formatter() {
                @Override
                public String format(Archive<?> archive) throws IllegalArgumentException {
                    String archPath = "";
                    List<String> names = new ArrayList<>();
                    Map<ArchivePath, Node> content = archive.getContent();
                    for (ArchivePath archivePath : content.keySet()) {
                        String p = archivePath.get();
                        if (p.endsWith("jar") ) {

                            int pos = p.lastIndexOf("/");
                            if (pos > 2) {
                                String name = p.substring(pos + 1);
                                names.add(name);

                                int n = name.lastIndexOf("-");
                                String jarName = name.substring(0,n);
                                String ver = name.substring(n + 1);
                                ver = ver.substring(0, ver.length() - 4);

                                if(data.containsKey(jarName)){
                                    List<String> jarVer = data.get(jarName);
                                    jarVer.add(ver);
                                }else{
                                    List<String> jarVer = new ArrayList<>();
                                    jarVer.add(ver);
                                    data.put(jarName, jarVer);
                                }
                            }
                        }
                    }
                    Collections.sort(names);
                    for(String n : names) {
                        archPath += n;
                        archPath += "\n";

                    }

                    //System.out.println(data);
                    return archPath;
                }
            });
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }


    }


}
