import { useSpring, useTransition, animated } from "react-spring";

exports.useSpringImpl = tuple => mkStyles => () => {
  const result = useSpring(mkStyles);
  return tuple(result[0])(result[1]);
};

exports.useTransitionImpl = useTransition;

exports.animatedComponentImpl = (name: string) => animated[name];

exports.animatedImpl = animated;
