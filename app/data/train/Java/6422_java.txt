package com.cetc;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.NamedThreadLocal;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import com.cetc.model.RespDataVo;
import com.cetc.utils.Base64;
import com.cetc.utils.DateUtil;
import com.cetc.utils.JsonUtils;
import com.cetc.utils.ValueTool;

import net.sf.json.JSONObject;

/**
 * 
 * @author hp
 *
 */
public class TokenInterceptor implements HandlerInterceptor {

	private static Logger logger = LoggerFactory.getLogger(TokenInterceptor.class);

	private NamedThreadLocal<Long> startTimeThreadLocal = new NamedThreadLocal<>("StopWatch-StartTime");

	@Override
	public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
			throws Exception {
		long beginTime = System.currentTimeMillis();// 1、开始时间
		startTimeThreadLocal.set(beginTime);// 线程绑定变量（该数据只有当前请求的线程可见）
		String reqJson = "";
		String getReqParm = "";
		
		//增加跨域请求
		if(request.getHeader("Origin")!=null&&
				(request.getHeader("Origin").equals("http://localhost:8000")||
			     request.getHeader("Origin").equals("http://10.111.10.42:8000")) 
			){
			logger.debug("需要跨域请求{}", request.getRequestURI());
			response.setHeader("Access-Control-Allow-Origin",request.getHeader("Origin"));
		};
		
		
		if (request.getRequestURI().contains("user/login")) {
			JSONObject obj = new JSONObject();
			obj.put("userName", request.getParameter("userName"));
			obj.put("userPwd", request.getParameter("userPwd"));
			logger.info("*****【" + request.getMethod() + "】请求url地址：" + request.getRequestURL() + " \n 请求参数： "
					+ obj.toString());
			return true;
		} else {
			RespDataVo root = new RespDataVo();
			String token = null;
			if (StringUtils.equals("get", request.getMethod().toLowerCase())) {
				token = request.getParameter("token");
				getReqParm = request.getQueryString();
			} else if (StringUtils.equals("post", request.getMethod().toLowerCase())) {
				InputStream inputStream = request.getInputStream();
				BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, "utf-8"));
				String tempStr = "";
				while ((tempStr = reader.readLine()) != null) {
					reqJson += tempStr;
				}
				reader.close();
				inputStream.close();
				JSONObject reqBody = JSONObject.fromObject(reqJson);
				token = reqBody.getString("token");
			}
			logger.info("*****【" + request.getMethod() + "】请求url地址：" + request.getRequestURL() + " \n 请求参数： "
					+ getReqParm + "" + reqJson);

			if (token != null) {
				String tokenStr = new String(Base64.decode(token));
				if (!tokenStr.contains("_") || tokenStr.split("_").length < 3) { // token
																					// 不包含"_"
																					// 或者包含但是格式不正确
					String code = "1012";
					root.setCode(code);
					response.setCharacterEncoding("utf-8");
					root.setMsg(ValueTool.getValue().get(code));
					response.getWriter().write(JsonUtils.parseBeanToJson(root));
					return false;
				} else {
					String tokenCode = tokenStr.split("_")[1];
					if (StringUtils.equals("A123456a", tokenStr.split("_")[2])) {// 校验token是否修改
						if (DateUtil.dayDiff(new Date(), tokenCode) - 30 > 0) {// 限制30分钟内可以访问
							String code = "1011";
							root.setCode(code);
							response.setCharacterEncoding("utf-8");
							root.setMsg(ValueTool.getValue().get(code));
							response.getWriter().write(JsonUtils.parseBeanToJson(root));
							return false;
						} else {
							request.setAttribute("userId", tokenStr.split("_")[0]);
							request.setAttribute("param", reqJson);
							return true;
						}
					} else {
						String code = "1012";
						root.setCode(code);
						response.setCharacterEncoding("utf-8");
						root.setMsg(ValueTool.getValue().get(code));
						response.getWriter().write(JsonUtils.parseBeanToJson(root));
						return false;
					}
				}

			}
			String code = "1010";
			root.setCode(code);
			response.setCharacterEncoding("utf-8");
			root.setMsg(ValueTool.getValue().get(code));
			response.getWriter().write(JsonUtils.parseBeanToJson(root));
			return false;
		}
	}

	@Override
	public void postHandle(HttpServletRequest request, HttpServletResponse response, Object handler,
			ModelAndView modelAndView) throws Exception {
	}

	@Override
	public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex)
			throws Exception {
		long endTime = System.currentTimeMillis();// 2、结束时间
		long beginTime = startTimeThreadLocal.get();// 得到线程绑定的局部变量（开始时间）
		long consumeTime = endTime - beginTime;// 3、消耗的时间
		if (consumeTime > 500) {// 此处认为处理时间超过500毫秒的请求为慢请求
			// TODO 记录到日志文件
			System.out.println(String.format("%s consume %d millis", request.getRequestURI(), consumeTime));
			logger.info("************end***********" + request.getRequestURL() + " 执行时长：" + consumeTime);
		}
	}
}