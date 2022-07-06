/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package model.interfacesdaoifs;

import java.sql.SQLException;
import java.util.List;
import model.exceptions.DaoException;
import model.values.Usuario;

/**
 *
 * @author Anderson Souza
 */
public interface UsuarioDaoIF {
    
    public boolean add(Usuario usuario) throws SQLException, ClassNotFoundException, DaoException;
    
    public boolean update(Usuario usuario, String email) throws SQLException, ClassNotFoundException;
    
    public boolean delete(String email) throws SQLException, ClassNotFoundException;
    
    public List<Usuario> buscaUsuarios() throws SQLException, ClassNotFoundException;
    
    public List<Usuario> buscaTodosOsUsuarioPorEmail(String email) throws SQLException, ClassNotFoundException;
    
    public Usuario buscaUsuarioPorEmail(String email) throws SQLException, ClassNotFoundException;
}
