import AltFingersRes from "src/AltFingers.mjs";

// This can be re-exported as is (no Fast-Refresh issues)
export { getStaticProps } from "src/AltFingers.mjs";

// Note:
// We need to wrap the make call with
// a Fast-Refresh conform function name,
// (in this case, uppercased first letter)
//
// If you don't do this, your Fast-Refresh will
// not work!
export default function AltFingers(props) {
  return <AltFingersRes {...props} />;
}
