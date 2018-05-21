package eu.europa.ec.fisheries.uvms.rules.rest.service.Arquillian;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.After;
import org.junit.Before;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.transaction.*;

public class TransactionalTests extends BuildRulesRestDeployment {

    @Inject
    protected UserTransaction userTransaction;

    @PersistenceContext
    protected EntityManager em;

    final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    @Before
    public void before() throws SystemException, NotSupportedException {
       // userTransaction.begin();
    }

    @After
    public void after() throws SystemException, HeuristicRollbackException, HeuristicMixedException, RollbackException {
        //userTransaction.commit();
    }

}

