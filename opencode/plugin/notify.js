export const NotifyPlugin = async ({ project, directory, $ }) => {
  const getProjectName = () => {
    if (project?.name) return project.name;
    const parts = directory.split('/');
    return parts[parts.length - 1] || 'opencode';
  };

  const projectName = getProjectName();

  return {
    event: async ({ event }) => {
      try {
        if (event.type === 'session.idle') {
          await $`curl -s -H "Title: [oc:${projectName}]" -H "Priority: default" -H "Tags: white_check_mark" -H "Click: android-app://com.termux" -d "Session completed! Ready for your next request." ntfy.sh/$NTFY_TOPIC > /dev/null`;
        }

        if (event.type === 'session.error') {
          await $`curl -s -H "Title: [oc:${projectName}]" -H "Priority: urgent" -H "Tags: fire" -H "Click: android-app://com.termux" -d "Session error occurred. Check the terminal for details." ntfy.sh/$NTFY_TOPIC > /dev/null`;
        }
      } catch (error) {
        console.error('Notification plugin error:', error.message);
      }
    },
  };
};
