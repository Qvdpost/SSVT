import { NodeMutator } from '.';
import * as types from '@babel/types';

enum AssignmentExpression {
  '-=' = '+=',
  '+=' = '-=',
}

export const uvaMutator: NodeMutator = {
  name: 'UvaMutator',

  *mutate(path) {
    if (path.isAssignmentExpression()  ) {
      yield types.assignmentExpression(AssignmentExpression[path.node.operator], path.node.left, path.node.right);
    }
  },
};


/*
enum UpdateOperators {
  '++' = '--',
  '--' = '++',
}

export const updateOperatorMutator: NodeMutator = {
  name: 'UpdateOperator',

  *mutate(path) {
    if (path.isUpdateExpression()) {
      yield types.updateExpression(UpdateOperators[path.node.operator], path.node.argument, path.node.prefix);
    }
  },
};

*/
