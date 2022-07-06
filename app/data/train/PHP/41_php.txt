<?php

use Symfony\Component\HttpKernel\Kernel;
use Symfony\Component\Config\Loader\LoaderInterface;

class AppKernel extends Kernel
{
    public function registerBundles()
    {
        $bundles = array(
            new Symfony\Bundle\FrameworkBundle\FrameworkBundle(),
            new Symfony\Bundle\SecurityBundle\SecurityBundle(),
            new Symfony\Bundle\TwigBundle\TwigBundle(),
            new Symfony\Bundle\MonologBundle\MonologBundle(),
            new Symfony\Bundle\SwiftmailerBundle\SwiftmailerBundle(),
            new Symfony\Bundle\DoctrineBundle\DoctrineBundle(),
            new Symfony\Bundle\AsseticBundle\AsseticBundle(),
            new Sensio\Bundle\FrameworkExtraBundle\SensioFrameworkExtraBundle(),
            new JMS\SecurityExtraBundle\JMSSecurityExtraBundle(),
            
            //Bundle ajouté
            new FOS\UserBundle\FOSUserBundle(),
            new Psa\PageBundle\PsaPageBundle(),
            //new Psa\UserBundle\PsaUserBundle(),
            new Psa\AdminBundle\PsaAdminBundle(),
            new Psa\CommonBundle\PsaCommonBundle(),
            
            //Forum Bundles
            new EWZ\Bundle\TimeBundle\EWZTimeBundle(),
            new WhiteOctober\PagerfantaBundle\WhiteOctoberPagerfantaBundle(),
            new CCDNComponent\CommonBundle\CCDNComponentCommonBundle(),
            new CCDNComponent\BBCodeBundle\CCDNComponentBBCodeBundle(),
            new CCDNComponent\CrumbTrailBundle\CCDNComponentCrumbTrailBundle(),
            new CCDNComponent\DashboardBundle\CCDNComponentDashboardBundle(),
            new CCDNComponent\AttachmentBundle\CCDNComponentAttachmentBundle(),
            new CCDNComponent\MenuBundle\CCDNComponentMenuBundle(),
            new CCDNForum\KarmaBundle\CCDNForumKarmaBundle(),
            new CCDNUser\UserBundle\CCDNUserUserBundle(),
            new CCDNForum\AdminBundle\CCDNForumAdminBundle(),
            new CCDNForum\ForumBundle\CCDNForumForumBundle(),
            new CCDNUser\AdminBundle\CCDNUserAdminBundle(),
            new EWZ\Bundle\RecaptchaBundle\EWZRecaptchaBundle(),
            new CCDNUser\ProfileBundle\CCDNUserProfileBundle(),
            new CCDNForum\ModeratorBundle\CCDNForumModeratorBundle(),
            new CCDNUser\MemberBundle\CCDNUserMemberBundle(),
            new CCDNMessage\MessageBundle\CCDNMessageMessageBundle(),

        );

        if (in_array($this->getEnvironment(), array('dev', 'test'))) {
            $bundles[] = new Acme\DemoBundle\AcmeDemoBundle();
            $bundles[] = new Symfony\Bundle\WebProfilerBundle\WebProfilerBundle();
            $bundles[] = new Sensio\Bundle\DistributionBundle\SensioDistributionBundle();
            $bundles[] = new Sensio\Bundle\GeneratorBundle\SensioGeneratorBundle();
            $bundles[] = new CoreSphere\ConsoleBundle\CoreSphereConsoleBundle();
        }
        
        return $bundles;
    }

    public function registerContainerConfiguration(LoaderInterface $loader)
    {
        $loader->load(__DIR__.'/config/config_'.$this->getEnvironment().'.yml');
    }
}
