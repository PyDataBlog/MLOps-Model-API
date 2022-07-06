/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package io.github.jass2125.locadora.jpa;

import javax.persistence.EntityManager;
import javax.persistence.Persistence;

/**
 *
 * @author Anderson Souza
 * @email jair_anderson_bs@hotmail.com
 * @since 2015, Feb 9, 2016
 */
public class EntityManagerJPA {
    private static EntityManager em;

    private EntityManagerJPA() {
    }
    
    
    public static EntityManager getEntityManager(){
        if(em == null) {
            em = Persistence.createEntityManagerFactory("default").createEntityManager();
        }
        return em;
    }
    

}
