<?php

namespace Gitlab\Clients;

class DeployKeyClient extends AbstractClient
{
    /**
     * @param  int    $projectId
     * @return \Psr\Http\Message\ResponseInterface
     */
    public function listDeployKeys($projectId)
    {
        return $this->getRequest(sprintf('projects/%u/keys', $projectId));
    }

    /**
     * @param  int $projectId
     * @param  int $keyId
     * @return \Psr\Http\Message\ResponseInterface
     */
    public function getDeployKey($projectId, $keyId)
    {
        return $this->getRequest(sprintf('projects/%u/keys/%u', $projectId, $keyId));
    }

    /**
     * @param  int        $projectId
     * @param  array|null $data
     * @return \Psr\Http\Message\ResponseInterface
     */
    public function createDeployKey($projectId, array $data = null)
    {
        return $this->postRequest(sprintf('projects/%u/keys', $projectId), $data);
    }

    /**
     * @param  int $projectId
     * @param  int $keyId
     * @return \Psr\Http\Message\ResponseInterface
     */
    public function deleteDeployKey($projectId, $keyId)
    {
        return $this->deleteRequest(sprintf('projects/%u/keys/%u', $projectId, $keyId));
    }
}
