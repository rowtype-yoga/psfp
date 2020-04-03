import { useSpring, useTransition, animated } from "react-spring";

exports.useSpringImpl = mkStyles => () => {
  const result = useSpring(mkStyles);
  return { style: result[0], set: result[1], stop: result[2] };
};

exports.useTransitionImpl = useTransition;

exports.animatedComponentImpl = (name: string) => animated[name];

exports.animatedImpl = animated;
