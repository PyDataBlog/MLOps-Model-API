/**
 * Created by Yang on 2/11/16.
 */
import { createSelector } from 'reselect';

const selectForgetPasswordPage = () => (state) => state.get('forgetpassword');

const selectEmail = () => createSelector(
  selectForgetPasswordPage(),
  (forgetPasswordState) => forgetPasswordState.get('email')
);

const selectError = () => createSelector(
  selectForgetPasswordPage(),
  (forgetPasswordState) => forgetPasswordState.get('error')
);

const selectResetEmailSent = () => createSelector(
  selectForgetPasswordPage(),
  (forgetPasswordState) => forgetPasswordState.get('resetEmailSent')
);

export {
  selectForgetPasswordPage,
  selectEmail,
  selectError,
  selectResetEmailSent,
};
