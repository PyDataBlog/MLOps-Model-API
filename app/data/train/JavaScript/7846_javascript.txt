export default function _isEdge() {
  const ua = navigator.userAgent.toLowerCase();
  return Boolean(ua.match(/edge/i));
}
