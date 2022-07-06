package dao;

import java.util.List;

import org.apache.ibatis.session.SqlSession;

import factory.FactoryService;
import vo.Bbs_CommmVo;
import vo.Board2Vo;

public class BoardDao2 {
	private static BoardDao2 dao;
	public static synchronized BoardDao2 getDao(){
		if ( dao == null ) dao = new BoardDao2();
		return dao;
	}
	
	public void insertDB(Board2Vo vo){
		SqlSession ss= FactoryService.getFatory().openSession(true);
		ss.insert("board2.in_board",vo);
		ss.close();
		
	}
	
	public List<Board2Vo> getList(){
		SqlSession ss= FactoryService.getFatory().openSession();
		List<Board2Vo> list=ss.selectList("board2.gList");
		ss.close();
		return list;
	}
	public Board2Vo view(int no){
		SqlSession ss= FactoryService.getFatory().openSession();
		Board2Vo vo =ss.selectOne("board2.view_board",no);
		ss.close();
		return vo;
	}
	
	// 댓글에 해당 되는 메소드
	public void commin(Bbs_CommmVo vo){
		SqlSession ss= FactoryService.getFatory().openSession(true);
		ss.insert("board2.bbscomin",vo);
		ss.close();
		
	}
	
	
	public List<Bbs_CommmVo> bbs_view(int no){
		SqlSession ss= FactoryService.getFatory().openSession();
		List<Bbs_CommmVo> list=ss.selectList("board2.comL",no);
		ss.close();
		return list;
	}
	
	public void replayInsert(Board2Vo vo){
		System.out.println("LOG replayInsert 메소드 시작 ");
		SqlSession ss= FactoryService.getFatory().openSession();
		try {
			ss.update("board2.replay_Update",vo);
			System.out.println("LOG replayUpdate");
			ss.insert("board2.replay_Insert",vo);
			System.out.println("LOG replayInsert");
			ss.commit();
		} catch (Exception e) {
			e.printStackTrace();
			ss.rollback();
		} finally{
			ss.close();			
			System.out.println("LOG replayInsert 메소드 끝 ");
		}
	}
	
	
	
	
	
}