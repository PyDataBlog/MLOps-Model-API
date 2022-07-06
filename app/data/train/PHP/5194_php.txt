<?php

namespace FSB\ASTT\CoreBundle\Repository;

use Doctrine\ORM\EntityRepository;

class TeamRepository extends EntityRepository
{
    public function findAllDisplayedSorteredByDates()
    {
        $qb = $this->_em->createQueryBuilder();
        
        $qb->select('t')
            ->from('FSB\ASTT\CoreBundle\Entity\Team', 't')
            ->where('t.deleted = :deleted')
            ->setParameter('deleted', false)
            ->orderBy('t.createdAt', 'DESC')
        ;
        
        return $qb->getQuery()->getResult();
    }
    public function findAllDisplayedByCivility($civility, $queryBuilderOnly = false)
    {
        $qb = $this->_em->createQueryBuilder();
        
        $qb->select('t')
            ->from('FSB\ASTT\CoreBundle\Entity\Team', 't')
            ->where('t.deleted = :deleted')
            ->andWhere('t.civility = :civility')
            ->setParameter('deleted', false)
            ->setParameter('civility', $civility)
            ->orderBy('t.civility', 'DESC')
            ->addOrderBy('t.number', 'ASC');
        ;
        
        if ($queryBuilderOnly) {
            return $qb;
        } else {
            return $qb->getQuery()->getResult();
        }
    }
    
    public function findAllDisplayed($queryBuilderOnly = false)
    {
        $qb = $this->_em->createQueryBuilder();
        
        $qb->select('t')
            ->from('FSB\ASTT\CoreBundle\Entity\Team', 't')
            ->where('t.deleted = :deleted')
            ->setParameter('deleted', false)
            ->orderBy('t.civility', 'DESC')
            ->addOrderBy('t.number', 'ASC')
        ;
        
        if ($queryBuilderOnly) {
            return $qb;
        } else {
            return $qb->getQuery()->getResult();
        }
    }
    
    public function find($id)
    {
        $qb = $this->_em->createQueryBuilder();
        
        $qb->select('t')
            ->from('FSB\ASTT\CoreBundle\Entity\Team', 't')
            ->where('t.deleted = :deleted')
            ->andWhere('t.id = :id')
            ->setParameter('deleted', false)
            ->setParameter('id', $id)
        ;

        try {
            $result = $qb->getQuery()->getSingleResult();
        }
        catch (\Doctrine\Orm\NoResultException $NRE) {
            $result = NULL;
        }

        return $result;
    }
    
    public function findOneById($id)
    {
        return $this->find($id);
    }
    
    public function findAll()
    {
        $qb = $this->_em->createQueryBuilder();
        
        $qb->select('t')
            ->from('FSB\ASTT\CoreBundle\Entity\Team', 't')
            ->where('t.deleted = :deleted')
            ->setParameter('deleted', false)
        ;
        
        return $qb->getQuery()->getResult();
    }
}