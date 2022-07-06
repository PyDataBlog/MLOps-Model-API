<?php

class shopKmphonemaskvalidatePlugin extends shopPlugin
{
    private $enable;
    private $where;

    private $phone_mask;
    private $phone_placeholder;
    private $validate;
    private $error_msg;
    private $phone_input_names;

    /***
     * @var waSmarty3View|waView
     */
    private $view;

    public function getAllSettings()
    {
        $this->enable = $this->getSettings('enable') ? 1 : 0;
        $this->where = $this->getSettings('where');

        $this->phone_mask = $this->getSettings('phone_mask');
        $this->phone_placeholder = $this->getSettings('phone_placeholder');
        $this->error_msg = $this->getSettings('error_msg');
        $this->error_msg = ifempty($this->error_msg, 'Неверный формат');
        $this->validate = $this->getSettings('validate') ? 1 : 0;
        $this->phone_input_names = $this->getSettings('phone_input_names');
        if (empty($this->phone_input_names)) {
            $this->phone_input_names = 'phone';
        }

        $this->view = wa()->getView();

        if (!empty($this->phone_input_names)) {
            $this->phone_input_names = explode(',', $this->getSettings('phone_input_names'));

            foreach ($this->phone_input_names as $i => $phone) {
                $this->phone_input_names[$i] = '[name*="[' . trim($phone) . ']"]';
            }
            $this->view->assign('phone_input_names', join(', ', $this->phone_input_names));
        } else {
            $this->phone_input_names = false;
        }

        $this->view->assign('where', $this->where);
        $this->view->assign('error_msg', $this->error_msg);
        $this->view->assign('phone_mask', $this->phone_mask);
        $this->view->assign('phone_placeholder', $this->phone_placeholder);
        $this->view->assign('validate', $this->validate);
    }

    public function frontendFooterHandler()
    {
        $this->getAllSettings();

        if ($this->enable && $this->where == 'everywhere' && $this->phone_input_names) {
            $this->addJs('js/jquery.mask.min.js', true);

            return $this->view->fetch($this->path . '/templates/PhoneMaskAndValidate.html');
        }
        return false;
    }

    public function frontendCheckoutHandler()
    {
        $this->getAllSettings();

        if ($this->enable && $this->where == 'checkout' && $this->phone_input_names) {
            $this->addJs('js/jquery.mask.min.js', true);

            return $this->view->fetch($this->path . '/templates/PhoneMaskAndValidate.html');
        }
        return false;
    }
}
