package configuration;

import org.agle4j.framework.constant.ConfigConstant;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.junit.Test;

/**
 * commons-configuration 包测试
 * 一个java应用程序的配置管理类库
 * 
 * @author hanyx
 * @since
 */
public class ConfigurationTest {

	@Test
	public void testConfiguration() {
		try {
			PropertiesConfiguration config = new PropertiesConfiguration(ConfigConstant.CONFIG_FILE) ;
			config.setProperty("colors.background", "#000000");
			config.save();
			Integer num = config.getInt("app.upload_limit") ;
			System.out.println(num);
		} catch (ConfigurationException e) {
			e.printStackTrace();
		}
	}
	
}
