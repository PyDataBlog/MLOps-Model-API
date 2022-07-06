package top.cardone.func.vx.usercenter.userDepartment;

import com.google.common.base.Charsets;
import lombok.extern.log4j.Log4j2;
import lombok.val;
import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.StopWatch;
import top.cardone.ConsumerApplication;
import top.cardone.context.ApplicationContextHolder;

import java.io.IOException;

@Log4j2
@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(classes = ConsumerApplication.class, value = {"spring.profiles.active=test"}, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
public class D0002FuncTest {
    @Value("http://localhost:${server.port:8765}${server.servlet.context-path:}/vx/usercenter/userDepartment/d0002.json")
    private String funcUrl;

    @Value("file:src/test/resources/top/cardone/func/vx/usercenter/userDepartment/D0002FuncTest.func.input.json")
    private Resource funcInputResource;

    @Value("file:src/test/resources/top/cardone/func/vx/usercenter/userDepartment/D0002FuncTest.func.output.json")
    private Resource funcOutputResource;

    private HttpEntity<String> httpEntity;

    private int pressure = 10000;

    @Before
    public void setup() throws Exception {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
        headers.set("Accept", MediaType.APPLICATION_JSON_UTF8_VALUE);
        headers.set("token", ApplicationContextHolder.getBean(org.apache.shiro.authc.credential.PasswordService.class).encryptPassword("admin"));
        headers.set("username", "admin");

        if (!funcInputResource.exists()) {
            FileUtils.write(funcInputResource.getFile(), "{}", Charsets.UTF_8);
        }

        String input = FileUtils.readFileToString(funcInputResource.getFile(), Charsets.UTF_8);

        httpEntity = new HttpEntity<>(input, headers);
    }

    @Test
    public void func() throws RuntimeException {
        String output = new org.springframework.boot.test.web.client.TestRestTemplate().postForObject(funcUrl, httpEntity, String.class);

        try {
            FileUtils.write(funcOutputResource.getFile(), output, Charsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void pressureFunc() throws Exception {
        for (int i = 0; i < pressure; i++) {
            val sw = new StopWatch();

            sw.start(funcUrl);

            new org.springframework.boot.test.web.client.TestRestTemplate().postForObject(funcUrl, httpEntity, String.class);

            sw.stop();

            if (sw.getTotalTimeMillis() > 500) {
                log.error(sw.prettyPrint());
            } else if (log.isDebugEnabled()) {
                log.debug(sw.prettyPrint());
            }

            log.debug("pressured:" + (i + 1));
        }
    }
}