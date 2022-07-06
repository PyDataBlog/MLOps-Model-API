<?php

namespace Onend\PayPal\Payment\Client;

use Guzzle\Http\Message\Response;

use Onend\PayPal\Common\Client\AbstractAuthenticatedClient;
use Onend\PayPal\Common\Enum\Endpoint;
use Onend\PayPal\Common\Enum\RequestFormat;
use Onend\PayPal\Payment\Model\Payment;
use Onend\PayPal\Payment\Model\PaymentList;

class PaymentClient extends AbstractAuthenticatedClient
{
    /**
     * Create a payment
     *
     * @param Payment $payment
     *
     * @return Payment
     */
    public function createPayment(Payment $payment)
    {
        $request = $this->post(
            Endpoint::CREATE_PAYMENT,
            [],
            $this->getSerializer()->serialize($payment, RequestFormat::JSON)
        );
        $response = $this->send($request);

        return $this->factoryPaymentResponse($response);
    }

    /**
     * Execute an approved PayPal payment by Payment
     *
     * @param Payment $payment
     *
     * @return Payment
     */
    public function executePayment(Payment $payment)
    {
        return $this->executePaymentByIds($payment->getId(), $payment->getPayer()->getPayerInfo()->getPayerId());
    }

    /**
     * Execute an approved PayPal payment by payerId and paymentId
     *
     * @param string $paymentId
     * @param string $payerId
     *
     * @return Payment
     */
    public function executePaymentByIds($paymentId, $payerId)
    {
        $data = json_encode(["payer_id" => $payerId]);
        $request = $this->post(Endpoint::EXECUTE_PAYMENT, null, $data, ["paymenId" => $paymentId]);
        $response = $this->send($request);

        return $this->factoryPaymentResponse($response);
    }

    /**
     * Look up a payment resource
     *
     * @param string $paymentId
     *
     * @return Payment
     */
    public function lookupPayment($paymentId)
    {
        $request = $this->get(Endpoint::LOOKUP_PAYMENT, null, ["paymenId" => $paymentId]);
        $response = $this->send($request);

        return $this->factoryPaymentResponse($response);
    }

    /**
     * @return PaymentList
     */
    public function listPayments()
    {
        $request = $this->get(Endpoint::LIST_PAYMENTS);
        $response = $this->send($request);

        return $this->getSerializer()->deserialize(
            $response->getBody(true),
            PaymentList::getClass(),
            RequestFormat::JSON
        );
    }

    /**
     * @param $response
     *
     * @return Payment
     */
    protected function factoryPaymentResponse(Response $response)
    {
        return $this->getSerializer()->deserialize($response->getBody(true), Payment::getClass(), RequestFormat::JSON);
    }
}
